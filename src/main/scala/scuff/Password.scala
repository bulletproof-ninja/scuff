package scuff

import java.util.Arrays
import java.nio.charset.Charset

/**
 * Password representation that stores a digested, non-reversible,
 * version of a password.
 * <p>This class is immutable.
 * <p>See <a href="http://java.sun.com/j2se/1.5.0/docs/guide/security/CryptoSpec.html#AppA">
 * Java Cryptography Reference</a> for standard names of digest algorithms.
 * @author Nils Kilden-Pedersen
 * @see java.security.MessageDigest
 */
final class Password(passwordDigest: Array[Byte], val algorithm: String, saltBytes: Array[Byte]) extends Serializable {
  require(passwordDigest != null, "Digest cannot be null")
  require(algorithm != null, "Algorithm cannot be null")

  private val digested = passwordDigest.clone // Defensive copy on receipt
  private val salty = saltBytes.clone

  /**
   * The password digest.
   * @return Digest bytes.
   */
  def digest = digested.clone // Defensive copy on hand-out
  def salt = salty.clone  

  def length = digested.length

  override def equals(obj: Any) = obj match {
    case that: Password ⇒ this.algorithm.equalsIgnoreCase(that.algorithm) && Arrays.equals(this.digested, that.digested) && Arrays.equals(this.salty, that.salty)
    case _ ⇒ false
  }

  override val hashCode = Arrays.hashCode(passwordDigest)

  /**
   * Check if password string matches this password.
   * @param Clear text password
   * @return `true` if it's a match
   */
  def matches(password: String) = Arrays.equals(this.digested, Password.digestion(password, salty, algorithm))

  override def toString = "Password(algorithm=\"%s\", length=%d)".format(algorithm, length)
}

object Password {

  private val randomizer = new java.security.SecureRandom

  def randomSalt(size: Int): Array[Byte] = {
    val array = new Array[Byte](size)
    randomizer.nextBytes(array)
    array
  }

  val DefaultAlgo = "SHA-256"

  /**
   * Construct from clear text password and salt using provided digest algorithm.
   * @param password Clear text password.
   * @param salt Password salt
   * @param algorithm Digest algorithm to use
   */
  def apply(password: String, salt: Array[Byte], algorithm: String) = new Password(digestion(password, salt, algorithm), algorithm, salt)
  /**
   * Construct from clear text password and salt using default digest algorithm.
   * @param password Clear text password.
   * @param salt Password salt
   */
  def apply(password: String, salt: Array[Byte]) = new Password(digestion(password, salt, DefaultAlgo), DefaultAlgo, salt)
  /**
   * Construct from clear text password and desired digest algorithm, no salt.
   * @param password Clear text password.
   * @param algorithm Digest algorithm to use
   */
  def apply(password: String, algorithm: String) = new Password(digestion(password, Array.empty, algorithm), algorithm, Array.empty)
  /**
   * Construct from clear text password using default digest algorithm, no salt.
   * @param password Clear text password.
   * @param algorithm Digest algorithm to use
   */
  def apply(password: String) = new Password(digestion(password, Array.empty, DefaultAlgo), DefaultAlgo, Array.empty)

  private val charset = Charset.forName("UTF-8")

  private def digestion(password: String, salt: Array[Byte], algo: String): Array[Byte] = {
    val md = java.security.MessageDigest.getInstance(algo)
    md.update(salt)
    md.digest(password.getBytes(charset))
  }
}
