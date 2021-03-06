package scuff

import org.junit._, Assert._
import scuff.crypto.CipherCodec
import java.security.InvalidKeyException

object TestCrypto {

  case class FooBar(str: String, int: Int, doubles: Map[String, Double])

}

class TestCrypto {

  import TestCrypto._

  val ArrayPrinter = new Codec[Array[Byte], Array[Byte]] {
    val base64 = Base64.RFC_2045(true, true)
    def encode(arr: Array[Byte]): Array[Byte] = {
//      println(s"Encoding ${arr.length} bytes:\n${base64 encode arr}")
      arr
    }
    def decode(arr: Array[Byte]): Array[Byte] = {
//      println(s"Decoding ${arr.length} bytes:\n${base64 encode arr}")
      arr
    }
  }

  @Test
  def aes(): Unit = {
    val aes = crypto.CipherCodec.generateAES()
    val codec = JavaSerializer[FooBar].pipe(aes).pipe(ArrayPrinter).pipe(Base64.RFC_4648)
    val fooBar = FooBar("Hello, World!", 42, Map(
        "inf" -> Double.PositiveInfinity,
        "max" -> Double.MaxValue,
        "one" -> 1d,
        "rando" -> crypto.SecureRandom.nextDouble()))
    val encrypted = codec.encode(fooBar)
    val decrypted = codec.decode(encrypted)
    assertEquals(fooBar, decrypted)
  }

  @Test
  def rsa(): Unit = {
    val aes = crypto.CipherCodec.generateAES(256)
    val rsa = crypto.CipherCodec.generateRSA(2048)
    val codec = rsa.pipe(ArrayPrinter).pipe(Base64.RFC_4648)
    val encryptedAES = codec encode aes.encryptionKey.getEncoded
    val decryptedAES = codec decode encryptedAES
    assertArrayEquals(aes.encryptionKey.getEncoded, decryptedAES)
  }

  @Test
  def twofish(): Unit = {
    val aes = crypto.CipherCodec.generateAES(256)
    val tf = crypto.CipherCodec.symmetric("Twofish/CBC/PKCS5Padding", 128)
    val codec = tf.pipe(ArrayPrinter).pipe(Base64.RFC_4648)
    val encryptedAES = codec encode aes.encryptionKey.getEncoded
    val decryptedAES = codec decode encryptedAES
    assertArrayEquals(aes.encryptionKey.getEncoded, decryptedAES)
  }

  @Test
  def aesCustomKey(): Unit = {
    val aesKey = CipherCodec.generateSecretKey("AES", 256)

    val aes1 = crypto.CipherCodec(aesKey, () => crypto.CipherCodec.newAESCipher)
    val codec1 = JavaSerializer[FooBar].pipe(aes1).pipe(ArrayPrinter).pipe(Base64.RFC_4648)

    val aes2 = crypto.CipherCodec(aesKey, () => crypto.CipherCodec.newAESCipher)
    val codec2 = JavaSerializer[FooBar].pipe(aes2).pipe(ArrayPrinter).pipe(Base64.RFC_4648)

    val fooBar = FooBar("Hello, World!", 42, Map(
        "inf" -> Double.PositiveInfinity,
        "max" -> Double.MaxValue,
        "one" -> 1d,
        "rando" -> crypto.SecureRandom.nextDouble()))
    val encrypted = codec1.encode(fooBar)
    val decrypted = codec2.decode(encrypted)
    assertEquals(fooBar, decrypted)
  }

  @Test
  def failFast(): Unit = {
    val bytes = scuff.crypto.SecureRandom.generateSeed(127)
    val invalidKey = new javax.crypto.spec.SecretKeySpec(bytes, "AES")
    try {
      val codec = crypto.CipherCodec.AES(invalidKey)
      fail(s"Should fail on invalid key length: $codec")
    } catch {
      case e: InvalidKeyException =>
        assertTrue(e.getMessage contains bytes.length.toString)
    }

  }
}
