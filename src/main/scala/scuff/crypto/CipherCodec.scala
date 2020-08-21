package scuff.crypto

import java.security.{ Key, KeyPair, KeyPairGenerator }
import javax.crypto.{ Cipher, SecretKey, KeyGenerator }
import javax.crypto.spec.IvParameterSpec
import scuff.concurrent.ResourcePool
import java.util.Arrays
import scuff._

/**
 * Cipher codec.
 */
final class CipherCodec private (
    val encryptionKey: Key,
    decryptionKey: Key,
    newCipher: () => Cipher)
  extends Codec[Array[Byte], Array[Byte]] {

  private[this] val cipherPool = ResourcePool(newCipher(), description = encryptionKey.getAlgorithm)
  private[this] val blockSize = newCipher().getBlockSize // Can be 0
  private[this] def randomIV() =
    if (blockSize == 0) Array.emptyByteArray
    else {
      val arr = new Array[Byte](blockSize)
      SecureRandom nextBytes arr
      arr
    }
  private[this] def ivSpec(iv: Array[Byte]) =
    if (iv.length == 0) null
    else new IvParameterSpec(iv)

  /** Before encryption. */
  private def paddedArray(unencrypted: Array[Byte]): Array[Byte] =
    if (blockSize == 0) unencrypted
    else {
      val padding = blockSize - (unencrypted.length % blockSize)
      val padded = new Array[Byte](unencrypted.length + padding)
      System.arraycopy(unencrypted, 0, padded, 0, unencrypted.length)
      Arrays.fill(padded, unencrypted.length, padded.length, padding.toByte)
      padded
    }

  /** After decryption. */
  private def unpaddedArray(decrypted: Array[Byte]): Array[Byte] =
    if (blockSize == 0) decrypted
    else {
      val padding = decrypted(decrypted.length - 1).unsigned
      val unpadded = new Array[Byte](decrypted.length - padding)
      System.arraycopy(decrypted, 0, unpadded, 0, unpadded.length)
      unpadded
    }

  def encode(bytes: Array[Byte]): Array[Byte] = {
    val padded = paddedArray(bytes)
    val iv = randomIV()
    cipherPool.use { cipher =>
      cipher.init(Cipher.ENCRYPT_MODE, encryptionKey, ivSpec(iv))
      iv ++ cipher.doFinal(padded)
    }
  }

  def decode(encrypted: Array[Byte]): Array[Byte] = {
    val iv = encrypted take blockSize
    val decrypted =
      cipherPool.use { cipher =>
        cipher.init(Cipher.DECRYPT_MODE, decryptionKey, ivSpec(iv))
        cipher.doFinal(encrypted drop blockSize)
      }
    unpaddedArray(decrypted)
  }

}

object CipherCodec {

  private[this] val verify: CipherCodec => CipherCodec = {
    val rando = SecureRandom.generateSeed(32)
    (codec: CipherCodec) => {
      val encoded = codec encode rando
      val decoded = codec decode encoded
      require(rando sameElements decoded, s"Must encode/decode correctly")
      codec
    }
  }

  /**
    * Ad hoc secret key generation.
    *
    * @param algo Key algorithm
    * @param keySize Key size
    * @return new random secret key
    */
  def generateSecretKey(algo: String, keySize: Int): SecretKey = {
    val keyGen = KeyGenerator.getInstance(algo)
    keyGen.init(keySize, SecureRandom)
    keyGen.generateKey()
  }

  /**
    * Ad hoc key pair generation.
    *
    * @param algo Key pair algorithm
    * @param keySize Key size
    * @return new random key pair
    */
  def generateKeyPair(algo: String, keySize: Int): KeyPair = {
    val keyGen = KeyPairGenerator.getInstance(algo)
    keyGen.initialize(keySize, SecureRandom)
    keyGen.generateKeyPair()
  }

  def newCipher(algo: String): Cipher = javax.crypto.Cipher.getInstance(algo)
  def newAESCipher: Cipher = newCipher("AES/CBC/PKCS5Padding")

  /** Generate ad-hoc symmetric cipher. */
  def symmetric(algo: String, keySize: Int): CipherCodec = {
    val keyAlgo = algo.split("/")(0)
    apply(generateSecretKey(keyAlgo, keySize), () => newCipher(algo))
  }

  /** Generate ad-hoc asymmetric cipher. */
  def asymmetric(algo: String, keySize: Int): CipherCodec = {
    val keyAlgo = algo.split("/")(0)
    apply(generateKeyPair(keyAlgo, keySize), () => newCipher(algo))
  }

  def AES(aesKey: SecretKey): CipherCodec = {
    require(aesKey.getAlgorithm startsWith "AES", s"Must be AES key, was: ${aesKey.getAlgorithm}")
    apply(aesKey, () => CipherCodec.newAESCipher)
  }

  /**
   * Generate ad-hoc AES cipher codec.
   * @param keySize Valid values are 128, 192, 256; defaults to 256
   */
  def generateAES(keySize: Int = 256): CipherCodec =
    symmetric("AES/CBC/PKCS5Padding", keySize)

  /**
   * Generate ad-hoc RSA cipher codec.
   * @param keySize For RSA, it's not recommended to use below 1024; defaults to 2048
   */
  def generateRSA(keySize: Int = 2048): CipherCodec =
    asymmetric("RSA", keySize)

  /**
    * @param keyPair  Asymmetric key pair
    * @param newCihper New `Cipher` function, which must return a new unique instance
    */
  def apply(keyPair: KeyPair, newCipher: () => Cipher): CipherCodec =
    verify {
      new CipherCodec(keyPair.getPublic, keyPair.getPrivate, newCipher)
    }

  /**
   * @param symmetricKey Symmetric key
   * @param newCihper New `Cipher` function, which must return a new unique instance
   */
  def apply(symmetricKey: SecretKey, newCipher: () => Cipher): CipherCodec =
    verify {
      new CipherCodec(symmetricKey, symmetricKey, newCipher)
    }

  /**
    * @param symmetricKey Symmetric key
    * @return
    */
  def apply(symmetricKey: SecretKey): CipherCodec =
    apply(symmetricKey, () => Cipher.getInstance(symmetricKey.getAlgorithm))

  /**
    * Create ad hoc `CipherCodec`.
    * @param algorithm
    * @param keySize
    * @return
    */
  def generate(algorithm: String, keySize: Int): CipherCodec =
    apply(generateSecretKey(algorithm, keySize))

}
