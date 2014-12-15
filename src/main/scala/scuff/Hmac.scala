package scuff

import javax.crypto._
import java.security.spec.AlgorithmParameterSpec
import java.util.Arrays
import java.security.SignatureException

class Hmac[A] (
  delegate: Codec[A, Array[Byte]],
  secretKey: SecretKey,
  macAlgorithm: String = "HmacSHA1",
  macAlgoParmSpec: AlgorithmParameterSpec = null)
    extends Codec[A, Array[Byte]] {

  private[this] val macPool = new ResourcePool(newMac)
  private def newMac: Mac = {
    val mac = Mac.getInstance(macAlgorithm)
    macAlgoParmSpec match {
      case null => mac.init(secretKey)
      case spec => mac.init(secretKey, spec)
    }
    mac
  }

  def encode(a: A): Array[Byte] = {
    val rawBytes = delegate.encode(a)
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
      delegate.decode(rawBytes)
    } else {
      throw new SignatureException(s"Key has changed or input data has been modified.")
    }
  }
}
