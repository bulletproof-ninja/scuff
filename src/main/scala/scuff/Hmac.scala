package scuff

import javax.crypto._
import java.security.spec.AlgorithmParameterSpec
import java.util.Arrays
import java.security.SignatureException
import java.nio.charset.Charset

object Hmac {

  final val DefaultAlgorithm = "HmacSHA1"

  def apply[A](
    delegate: Codec[A, Array[Byte]],
    key: SecretKey,
    macAlgorithm: String,
    macAlgoParmSpec: AlgorithmParameterSpec): Hmac[A] =
    new Hmac[A] {
      protected val codec = delegate
      protected val secretKey = key
      override protected def macAlgo = macAlgorithm
      override protected def macAlgoSpec = macAlgoParmSpec
    }
  def apply[A](
    delegate: Codec[A, Array[Byte]],
    key: SecretKey): Hmac[A] = apply(delegate, key, DefaultAlgorithm, null)
  def apply[A](
    delegate: Codec[A, Array[Byte]],
    key: SecretKey,
    macAlgorithm: String): Hmac[A] = apply(delegate, key, macAlgorithm, null)

  private[this] val UTF8 = Charset.forName("UTF-8")

  def apply[A](
    delegate: Codec[A, String],
    key: SecretKey,
    macAlgorithm: String = DefaultAlgorithm,
    macAlgoParmSpec: AlgorithmParameterSpec = null,
    charset: Charset = UTF8): Hmac[A] = {
    val byteCodec = new Codec[A, Array[Byte]] {
      def encode(a: A): Array[Byte] = delegate.encode(a).getBytes(charset)
      def decode(b: Array[Byte]): A = delegate.decode(new String(b, charset))
    }
    apply(byteCodec, key, macAlgorithm, macAlgoParmSpec)
  }

  def generateKey(bitSize: Int = 512, algorithm: String = DefaultAlgorithm): SecretKey = {
    val keyGen = javax.crypto.KeyGenerator.getInstance(algorithm)
    keyGen.init(bitSize)
    keyGen.generateKey()
  }

}

abstract class Hmac[A] extends Serializer[A] {

  protected val codec: Serializer[A]
  protected val secretKey: SecretKey
  protected def macAlgo: String = Hmac.DefaultAlgorithm
  protected def macAlgoSpec: AlgorithmParameterSpec = null

  private[this] val macPool = new ResourcePool(newMac)
  private def newMac: Mac = {
    val mac = Mac.getInstance(macAlgo)
    macAlgoSpec match {
      case null => mac.init(secretKey)
      case spec => mac.init(secretKey, spec)
    }
    mac
  }

  def encode(a: A): Array[Byte] = {
    val rawBytes = codec.encode(a)
    val macBytes = macPool.borrow(_.doFinal(rawBytes))
    val output = new Array[Byte](4 + rawBytes.length + macBytes.length)
    Numbers.intToBytes(rawBytes.length, output)
    System.arraycopy(rawBytes, 0, output, 4, rawBytes.length)
    System.arraycopy(macBytes, 0, output, 4 + rawBytes.length, macBytes.length)
    output
  }

  private def bytesMatch(arr1: Array[Byte], arr2: Array[Byte], arr2Offset: Int): Boolean = {
    if (arr1.length != arr2.length - arr2Offset) false else {
      var idx = 0
      while (idx != arr1.length) {
        if (arr1(idx) != arr2(idx + arr2Offset)) return false
        idx += 1
      }
      true
    }
  }

  @throws[SignatureException]
  def decode(arr: Array[Byte]): A = {
    val rawByteLen = Numbers.bytesToInt(arr)
    val macOffset = 4 + rawByteLen
    val rawBytes = Arrays.copyOfRange(arr, 4, macOffset)
    val newMac = macPool.borrow(_.doFinal(rawBytes))
    if (bytesMatch(newMac, arr, macOffset)) {
      codec.decode(rawBytes)
    } else {
      throw new SignatureException(s"Key has changed or input data has been modified.")
    }
  }
}
