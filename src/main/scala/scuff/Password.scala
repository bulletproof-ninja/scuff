package scuff

import java.util.Arrays
import java.nio.charset.Charset
import scala.concurrent.duration._

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
  *   4. For workFactor > 1, repeat from 2, using the digest output as input
  * @author Nils Kilden-Pedersen
  * @see java.security.MessageDigest
  */
final class Password(passwordDigest: Array[Byte], val algorithm: String, saltBytes: Array[Byte], val workFactor: Int)
    extends Serializable {
  require(passwordDigest != null, "Digest cannot be null")
  require(algorithm != null, "Algorithm cannot be null")
  require(workFactor > 0, s"Must have a work factor of at least one, not $workFactor")

  private val _digest = passwordDigest.clone // Defensive copy
  private val _salt = saltBytes.clone // Defensive copy

  /**
    * The password digest.
    * @return Copy of digest.
    */
  def digest = _digest.clone // Defensive copy
  /**
    * The password salt.
    * @return Copy of salt.
    */
  def salt = _salt.clone // Defensive copy

  /** Digest length in bytes. */
  def length = _digest.length

  override def equals(that: Any) = that match {
    case that: Password => (this eq that) || {
      this.workFactor == that.workFactor &&
        this.algorithm.equalsIgnoreCase(that.algorithm) &&
        Arrays.equals(this._salt, that._salt) &&
        Arrays.equals(this._digest, that._digest)
    }
    case _ => false
  }
  override val hashCode = Arrays.hashCode(passwordDigest)

  /**
    * Check if password string matches this password.
    * @param Clear text password
    * @return `true` if it's a match, `false` otherwise
    */
  def matches(password: String) = {
    val (compareDigest, _) = Password.digestion(password, _salt, algorithm, Left(workFactor))
    Arrays.equals(_digest, compareDigest)
  }

  override def toString = "Password(algorithm=\"%s\", length=%d, workFactor=%d)".format(algorithm, length, workFactor)
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
    val (bytes, workFactor) = digestion(password, salt, config.algorithm, config.work)
    new Password(bytes, config.algorithm, salt, workFactor)
  }

  private val charset = Charset.forName("UTF-8")

  private def digestion(password: String, salt: Array[Byte], algo: String, iterative: Either[Int, FiniteDuration]): (Array[Byte], Int) = {
    val md = MessageDigest.getInstance(algo)
    val passwordBytes = password.getBytes(charset)
    iterative match {
      case Left(workFactor) =>
        var soFar = 0
        val result = digestUntil(md, salt, passwordBytes) {
          soFar += 1
          soFar == workFactor
        }
        result -> workFactor
      case Right(duration) =>
        val minMillis = duration.toMillis
        val started = System.currentTimeMillis()
        var workFactor = 0
        val result = digestUntil(md, salt, passwordBytes) {
          workFactor += 1
          System.currentTimeMillis - started >= minMillis
        }
        result -> workFactor
    }
  }
  @annotation.tailrec
  private def digestUntil(md: MessageDigest, salt: Array[Byte], workingBytes: Array[Byte])(done: => Boolean): Array[Byte] = {
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
    * @param work Defines how to digest iteratively. Can be either a fixed number (at least 1),
    * or a minimum duration (don't go overboard here). Using a duration will be adaptive to the hardware
    * it's running on, and makes digestion time more predictable.
    */
  case class Config(algorithm: String, saltLength: Int, work: Either[Int, FiniteDuration]) {
    require(MessageDigest.getInstance(algorithm) != null)
    require(saltLength >= 0, "Negative salt length is nonsensical")
    work match {
      case Left(workFactor) => require(workFactor > 0, "Must have a work factor of at least one, not " + workFactor)
      case Right(duration) => require(duration.isFinite, "Must be a finite duration: " + duration)
    }
    /**
      * @param algorithm The digest algorithm. This string must be understood by [[java.security.MessageDigest]]
      * @param saltLength The length of the random salt generated. Can be 0 for no salt.
      * @param workFactor Number of times to iterate the digest. Must be at least 1.
      */
    def this(algorithm: String, saltLength: Int, workFactor: Int) = this(algorithm, saltLength, Left(workFactor))
    /**
      * @param algorithm The digest algorithm. This string must be understood by [[java.security.MessageDigest]]
      * @param saltLength The length of the random salt generated. Can be 0 for no salt.
      * @param workDuration Minimum amount of time spent on iterative digestion
      */
    def this(algorithm: String, saltLength: Int, workDuration: FiniteDuration) = this(algorithm, saltLength, Right(workDuration))
  }

}
