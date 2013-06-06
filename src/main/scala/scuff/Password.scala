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

  override def toString = "Password(algorithm=\"%s\", length=%d)".format(algorithm, length)
}

object Password {
  import java.security.MessageDigest
  private val randomizer = new java.security.SecureRandom

  def randomSalt(size: Int): Array[Byte] = {
    val array = new Array[Byte](size)
    randomizer.nextBytes(array)
    array
  }

  final val DefaultAlgo = "SHA-256"
  private val DefaultIterations = Left(1)

  /**
   * Construct from clear text password and salt using provided digest algorithm.
   * @param password Clear text password.
   * @param salt Password salt
   * @param algorithm Digest algorithm to use
   * @param fixedIterations The fixed number of iterations spent digesting password
   */
  def apply(password: String, salt: Array[Byte], algorithm: String, fixedIterations: Int) = {
    val (bytes, iterations) = digestion(password, salt, algorithm, Left(fixedIterations))
    new Password(bytes, DefaultAlgo, salt, iterations)
  }

  /**
   * Construct from clear text password and salt using provided digest algorithm.
   * @param password Clear text password.
   * @param salt Password salt
   * @param algorithm Digest algorithm to use
   * @param minDuration The minimum time to iteratively digesting password
   */
  def apply(password: String, salt: Array[Byte], algorithm: String, minDuration: Duration) = {
    val (bytes, iterations) = digestion(password, salt, algorithm, Right(minDuration))
    new Password(bytes, DefaultAlgo, salt, iterations)
  }

  /**
   * Construct from clear text password and salt using provided digest algorithm.
   * @param password Clear text password.
   * @param salt Password salt
   * @param minDuration The minimum time to iteratively digesting password
   */
  def apply(password: String, salt: Array[Byte], minDuration: Duration) = {
    val (bytes, iterations) = digestion(password, salt, DefaultAlgo, Right(minDuration))
    new Password(bytes, DefaultAlgo, salt, iterations)
  }
  /**
   * Construct from clear text password and salt using provided digest algorithm.
   * @param password Clear text password.
   * @param salt Password salt
   * @param fixedIterations The fixed number of iterations spent digesting password
   */
  def apply(password: String, salt: Array[Byte], fixedIterations: Int) = {
    val (bytes, iterations) = digestion(password, salt, DefaultAlgo, Left(fixedIterations))
    new Password(bytes, DefaultAlgo, salt, iterations)
  }

  /**
   * Construct from clear text password and salt using provided digest algorithm.
   * @param password Clear text password.
   * @param salt Password salt
   * @param algorithm Digest algorithm to use
   */
  def apply(password: String, salt: Array[Byte], algorithm: String) = {
    val (bytes, iterations) = digestion(password, salt, algorithm, DefaultIterations)
    new Password(bytes, algorithm, salt, iterations)
  }
  /**
   * Construct from clear text password and salt using default digest algorithm.
   * @param password Clear text password.
   * @param salt Password salt
   */
  def apply(password: String, salt: Array[Byte]) = {
    val (bytes, iterations) = digestion(password, salt, DefaultAlgo, DefaultIterations)
    new Password(bytes, DefaultAlgo, salt, iterations)
  }
  /**
   * Construct from clear text password and desired digest algorithm, no salt.
   * @param password Clear text password.
   * @param algorithm Digest algorithm to use
   */
  def apply(password: String, algorithm: String) = {
    val (bytes, iterations) = digestion(password, Array.empty, algorithm, DefaultIterations)
    new Password(bytes, algorithm, Array.empty, iterations)
  }
  /**
   * Construct from clear text password using default digest algorithm, no salt.
   * @param password Clear text password.
   * @param algorithm Digest algorithm to use
   */
  def apply(password: String): Password = apply(password, Array[Byte]())

  private val charset = Charset.forName("UTF-8")

  private def digestion(password: String, salt: Array[Byte], algo: String, iterations: Either[Int, Duration]): (Array[Byte], Int) = {
    val md = MessageDigest.getInstance(algo)
    val passwordBytes = password.getBytes(charset)
    iterations match {
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
}
