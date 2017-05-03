package scuff

import javax.crypto._
import java.security.spec.AlgorithmParameterSpec
import java.util.Arrays
import java.security.SignatureException

object Hmac {

  final val DefaultAlgorithm = "HmacSHA1"

  def apply[A](
    serializer: Serializer[A],
    hmac: HmacFunction): Hmac[A, Array[Byte]] =
    new BinaryHmac(serializer, hmac)

  def apply[A](
    serializer: Serializer[A],
    key: SecretKey): Hmac[A, Array[Byte]] =
    apply(serializer, new HmacFunction(key))

  def apply[A, Z](
    toBytes: A => Array[Byte],
    combinerSplitter: Codec[(A, Array[Byte]), Z],
    hmacHasher: HmacFunction): Hmac[A, Z] =
    new CustomHmac(
      Codec.noop, toBytes, hmacHasher,
      Codec.noop, combinerSplitter)

  def apply[A, Z](
    toBytes: A => Array[Byte],
    combinerSplitter: Codec[(A, Array[Byte]), Z],
    key: SecretKey): Hmac[A, Z] =
    apply(toBytes, combinerSplitter, new HmacFunction(key))

  def json(hmac: HmacFunction): Hmac[String, String] =
    json[String](Codec.noop, hmac)
  def json[A](
    jsonCodec: Codec[A, String],
    hmac: HmacFunction): Hmac[A, String] = {
    new CustomHmac(
      jsonCodec, Codec.UTF8.encode, hmac,
      Codec.noop, JsonSplitterCombiner)
  }
  def base64[A](
    codec: Codec[A, Array[Byte]],
    hmac: HmacFunction,
    b64: Base64.Base64 = Base64.RFC_4648): Hmac[A, String] = {
    val splitterCombiner = new Codec[(Array[Byte], Array[Byte]), String] {
      def encode(arrays: (Array[Byte], Array[Byte])): String = {
        val single = ByteArraySplitterCombiner encode arrays
        (b64 encode single).toString
      }
      def decode(str: String): (Array[Byte], Array[Byte]) = {
        val single = b64 decode str
        ByteArraySplitterCombiner decode single
      }
    }
    new CustomHmac[A, Array[Byte], Array[Byte], String](
      codec, identity, hmac, Codec.noop, splitterCombiner)
  }

  def generateKey(bitSize: Int = 512, algorithm: String = DefaultAlgorithm): SecretKey = {
    val keyGen = javax.crypto.KeyGenerator.getInstance(algorithm)
    keyGen.init(bitSize)
    keyGen.generateKey()
  }

}

class HmacFunction(
  secretKey: SecretKey,
  macAlgo: String = Hmac.DefaultAlgorithm,
  macAlgoSpec: AlgorithmParameterSpec = null)
    extends (Array[Byte] => Array[Byte]) {

  private[this] val macPool = new concurrent.ResourcePool(newMac)
  private[this] def newMac: Mac = {
    val mac = Mac.getInstance(macAlgo)
    macAlgoSpec match {
      case null => mac.init(secretKey)
      case spec => mac.init(secretKey, spec)
    }
    mac
  }
  def apply(bytes: Array[Byte]): Array[Byte] = macPool.use(_.doFinal(bytes))

}

sealed abstract class Hmac[A, Z] extends Codec[A, Z] {
  protected final def verifyHash(h1: Array[Byte], h2: Array[Byte]): Unit = {
    if (!Arrays.equals(h1, h2)) {
      throw new SignatureException(s"Input data has been modified, or key has changed.")
    }
  }
}

private class BinaryHmac[A](
  serializer: Serializer[A], hmac: Array[Byte] => Array[Byte])
    extends Hmac[A, Array[Byte]] {
  def encode(a: A): Array[Byte] = {
    val bytes = serializer.encode(a)
    val hash = hmac(bytes)
    ByteArraySplitterCombiner.encode(bytes -> hash)
  }
  def decode(arr: Array[Byte]): A = {
    val (bytes, hash1) = ByteArraySplitterCombiner.decode(arr)
    val hash2 = hmac(bytes)
    verifyHash(hash1, hash2)
    serializer.decode(bytes)
  }
}
private class CustomHmac[A, B, H, Z](
  abCodec: Codec[A, B],
  toBytes: B => Array[Byte],
  hmac: HmacFunction,
  hashCodec: Codec[Array[Byte], H],
  splitterCombiner: Codec[(B, H), Z])
    extends Hmac[A, Z] {
  def encode(a: A): Z = {
    val b = abCodec encode a
    val bytes = toBytes(b)
    val hash = hmac(bytes)
    val h = hashCodec.encode(hash)
    splitterCombiner.encode(b -> h)
  }
  def decode(z: Z): A = {
    val (b, h) = splitterCombiner.decode(z)
    val hash1 = hashCodec.decode(h)
    val hash2 = hmac(toBytes(b))
    verifyHash(hash1, hash2)
    abCodec.decode(b)
  }
}

private object ByteArraySplitterCombiner
    extends Codec[(Array[Byte], Array[Byte]), Array[Byte]] {
  def encode(input: (Array[Byte], Array[Byte])): Array[Byte] = {
    val (a1, a2) = input
    val combined = new Array[Byte](4 + a1.length + a2.length)
    Numbers.intToBytes(a1.length, combined)
    System.arraycopy(a1, 0, combined, 4, a1.length)
    System.arraycopy(a2, 0, combined, 4 + a1.length, a2.length)
    combined
  }
  def decode(array: Array[Byte]): (Array[Byte], Array[Byte]) = {
    val a1EndPos = Numbers.bytesToInt(array) + 4
    val a1 = Arrays.copyOfRange(array, 4, a1EndPos)
    val a2 = Arrays.copyOfRange(array, a1EndPos, array.length)
    a1 -> a2
  }
}

private object JsonSplitterCombiner
    extends Codec[(String, Array[Byte]), String] {
  @inline private def b64 = Base64.RFC_4648
  def encode(tuple: (String, Array[Byte])): String = {
    val (data, hash) = tuple
    val b64Hash = b64.encode(hash)
    s"""{"data":$data,"hash":"$b64Hash"}"""
  }
  private def jsonDataPrefix = "{\"data\":"
  private def jsonHashPrefix = ",\"hash\":"
  def decode(json: String): (String, Array[Byte]) = {
    val commaPos = json.lastIndexOf(jsonHashPrefix)
    val data = json.substring(jsonDataPrefix.length, commaPos)
    val b64Hash = json.subSequence(commaPos + jsonHashPrefix.length + 1, json.length - 2)
    val hash = b64.decode(b64Hash)
    data -> hash
  }
}
