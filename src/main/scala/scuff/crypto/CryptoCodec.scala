package scuff.crypto

import java.security.{ Key, KeyPair, KeyPairGenerator }
import javax.crypto.{ Cipher, SecretKey, KeyGenerator }
import javax.crypto.spec.IvParameterSpec
import scuff.concurrent.UnboundedResourcePool
import java.util.Arrays
import scuff._

class CipherCodec private (val encryptionKey: Key, decryptionKey: Key, cipher: => Cipher)
  extends Codec[Array[Byte], Array[Byte]] {

  def this(keyPair: KeyPair, cipher: => Cipher) = this(keyPair.getPublic, keyPair.getPrivate, cipher)

  def this(symmetricKey: SecretKey, cipher: => Cipher) = this(symmetricKey, symmetricKey, cipher)

  private def this(symmetricKey: SecretKey) = this(symmetricKey, Cipher.getInstance(symmetricKey.getAlgorithm))

  def this(algorithm: String, keySize: Int) = this(CipherCodec.SecretKey(algorithm, keySize))

  private[this] val cipherPool = new UnboundedResourcePool(cipher)
  private[this] val blockSize = cipherPool.use(_.getBlockSize) // Can be 0
  private[this] val iv = if (blockSize == 0) null else new IvParameterSpec(SecureRandom.generateSeed(blockSize))

  private def paddedArray(arr: Array[Byte]): Array[Byte] =
    if (blockSize == 0) arr
    else {
      val padding = blockSize - (arr.length % blockSize)
      val padded = new Array[Byte](arr.length + padding)
      System.arraycopy(arr, 0, padded, 0, arr.length)
      Arrays.fill(padded, arr.length, padded.length, padding.toByte)
      padded
    }
  private def unpaddedArray(arr: Array[Byte]): Array[Byte] =
    if (blockSize == 0) arr
    else {
      val padding = arr(arr.length - 1).unsigned
      val unpadded = new Array[Byte](arr.length - padding)
      System.arraycopy(arr, 0, unpadded, 0, unpadded.length)
      unpadded
    }

  def encode(bytes: Array[Byte]): Array[Byte] = {
    val padded = paddedArray(bytes)

    cipherPool.use { cipher =>
      cipher.init(Cipher.ENCRYPT_MODE, encryptionKey, iv)
      cipher.doFinal(padded)
    }
  }

  def decode(encrypted: Array[Byte]): Array[Byte] = {
    val decrypted =
      cipherPool.use { cipher =>
        cipher.init(Cipher.DECRYPT_MODE, decryptionKey, iv)
        cipher.doFinal(encrypted)
      }
    unpaddedArray(decrypted)
  }

}

object CipherCodec {

  def SecretKey(algo: String, keySize: Int) = {
    val keyGen = KeyGenerator.getInstance(algo)
    keyGen.init(keySize, SecureRandom)
    keyGen.generateKey()
  }

  def KeyPair(algo: String, keySize: Int) = {
    val keyGen = KeyPairGenerator.getInstance(algo)
    keyGen.initialize(keySize, SecureRandom)
    keyGen.generateKeyPair()
  }

  def Cipher(algo: String): Cipher = javax.crypto.Cipher.getInstance(algo)

  /** Ad-hoc symmetric cipher. */
  def symmetric(algo: String, keySize: Int) = {
    val keyAlgo = algo.split("/")(0)
    new CipherCodec(SecretKey(keyAlgo, keySize), Cipher(algo))
  }

  /** Ad-hoc asymmetric cipher. */
  def asymmetric(algo: String, keySize: Int) = {
    val keyAlgo = algo.split("/")(0)
    new CipherCodec(KeyPair(keyAlgo, keySize), Cipher(algo))
  }

  /**
   * @param keySize Valid values are 128, 192, 256
   */
  def AES(keySize: Int = 256) = symmetric("AES/CBC/PKCS5Padding", keySize)

  /**
   * @param keySize For RSA, it's not recommended to use below 1024
   */
  def RSA(keySize: Int = 2048) = asymmetric("RSA", keySize)

}
