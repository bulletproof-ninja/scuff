package scuff

import java.util.Arrays
import java.nio.charset.Charset
import concurrent.duration._

/**
 * Password representation that stores a digested, non-reversible,
 * version of a password.
 * <p>This class is immutable.
 * <p>See <a href="http://java.sun.com/j2se/1.5.0/docs/guide/security/CryptoSpec.html#AppA">
 * Java Cryptography Reference</a> for standard names of digest algorithms.
  * <p>A String password is digested in the following manner:
  *   1. Convert to bytes using UTF-8 encoding
  *   2. Append salt
  *   3. Digest
  *   4. For iterations > 1, repeat from 2, using the digest output as input
 * @author Nils Kilden-Pedersen
 * @see java.security.MessageDigest
 */
final class Password(passwordDigest: Array[Byte], val algorithm: String, saltBytes: Array[Byte], val iterations: Int) extends Serializable {
  require(passwordDigest != null, "Digest cannot be null")
  require(algorithm != null, "Algorithm cannot be null")
  require(iterations > 0, "Must have at least one iteration: " + iterations)

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
    case that: Password ⇒
      this.iterations == that.iterations &&
        this.algorithm.equalsIgnoreCase(that.algorithm) &&
        Arrays.equals(this.salty, that.salty) &&
        Arrays.equals(this.digested, that.digested)
    case _ ⇒ false
  }

  override val hashCode = Arrays.hashCode(passwordDigest)

  /**
   * Check if password string matches this password.
   * @param Clear text password
   * @return `true` if it's a match
   */
  def matches(password: String) = {
    val (compareDigest, _) = Password.digestion(password, salty, algorithm, Left(iterations))
    Arrays.equals(this.digested, compareDigest)
  }

  override def toString = "Password(algorithm=\"%s\", length=%d, iterations=%d)".format(algorithm, length, iterations)
}

object Password {
  import java.security.MessageDigest
  private val randomizer = new java.security.SecureRandom

  private def randomSalt(size: Int): Array[Byte] = {
    val array = new Array[Byte](size)
    randomizer.nextBytes(array)
    array
  }

  def apply(password: String)(implicit config: Config): Password = {
    val salt = randomSalt(config.saltLength)
    val (bytes, iterations) = digestion(password, salt, config.algorithm, config.iterative)
    new Password(bytes, config.algorithm, salt, iterations)
  }

  private val charset = Charset.forName("UTF-8")

  private def digestion(password: String, salt: Array[Byte], algo: String, iterative: Either[Int, Duration]): (Array[Byte], Int) = {
    val md = MessageDigest.getInstance(algo)
    val passwordBytes = password.getBytes(charset)
    iterative match {
      case Left(iterations) ⇒
        var soFar = 0
        val result = digestUntil(md, salt, passwordBytes) {
          soFar += 1
          soFar == iterations
        }
        result -> iterations
      case Right(duration) ⇒
        val minMillis = duration.toMillis
        val started = System.currentTimeMillis()
        var iterations = 0
        val result = digestUntil(md, salt, passwordBytes) {
          iterations += 1
          System.currentTimeMillis - started >= minMillis
        }
        result -> iterations
    }
  }
  @annotation.tailrec
  private def digestUntil(md: MessageDigest, salt: Array[Byte], workingBytes: Array[Byte])(done: ⇒ Boolean): Array[Byte] = {
    md.update(salt)
    md.update(workingBytes)
    val digested = md.digest()
    if (done) {
      digested
    } else {
      digestUntil(md, salt, digested)(done)
    }
  }

  /**
    * Configuration.
    * @param algorithm The digest algorithm. This string must be understood by [[java.security.MessageDigest]]
    * @param saltLength The length of the random salt generated. Can be 0 for no salt.
    * @param iterative Defines how to digest iteratively. Can be either a fixed number (at least 1),
    * or a minimum duration (don't go overboard here). Using a duration will be adaptive to the hardware
    * it's running on, and makes digestion time more predictable.
    */
  case class Config(algorithm: String, saltLength: Int, iterative: Either[Int, Duration]) {
    require(MessageDigest.getInstance(algorithm) != null)
    require(saltLength >= 0, "Negative salt length is nonsensical")
    iterative match {
      case Left(iterations) ⇒ require(iterations > 0, "Must have at least one iteration: " + iterations)
      case Right(duration) ⇒ require(duration.isFinite, "Must be a finite duration: " + duration)
    }
    /**
      * @param algorithm The digest algorithm. This string must be understood by [[java.security.MessageDigest]]
      * @param saltLength The length of the random salt generated. Can be 0 for no salt.
      * @param iterations Number of times to iterate the digest. Must be at least 1.
      */
    def this(algorithm: String, saltLength: Int, iterations: Int) = this(algorithm, saltLength, Left(iterations))
    /**
      * @param algorithm The digest algorithm. This string must be understood by [[java.security.MessageDigest]]
      * @param saltLength The length of the random salt generated. Can be 0 for no salt.
      * @param digestDuration Minimum amount of time spent on iterative digestion
      */
    def this(algorithm: String, saltLength: Int, digestDuration: Duration) = this(algorithm, saltLength, Right(digestDuration))
  }

}
