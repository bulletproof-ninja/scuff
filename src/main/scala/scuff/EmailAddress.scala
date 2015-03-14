package scuff

/**
 * Simple email address, defined by user name and domain.
 * Although strictly incorrect, this class defines equality in a
 * case insensitive way, to comply with real-world usage.
 * <p>This class is immutable.
 * @author Nils Kilden-Pedersen
 */
final case class EmailAddress @throws(classOf[IllegalArgumentException]) (user: String, domain: String) extends Comparable[EmailAddress] {

  import EmailAddress._

  @throws(classOf[IllegalArgumentException])
  private def this(userDomain: (String, String)) = this(userDomain._1, userDomain._2)

  /**
   * Construct instance from single password string. E.g. "user@domain.com".
   * @param address Full email address as one string
   */
  @throws(classOf[IllegalArgumentException])
  def this(address: String) = this(EmailAddress.split(address))

  require(isValidLength(user, domain), "Address exceeds 254 characters: " + user)
  require(isValidUser(user), "Invalid user name: " + user)
  require(isValidDomain(domain), "Invalid domain name: " + domain)

  override val toString = user + "@" + domain
  val toLowerCase = toString.toLowerCase

  def compareTo(that: EmailAddress) = this.toLowerCase compareTo that.toLowerCase

  override def equals(any: Any) = any match {
    case that: EmailAddress => this.toLowerCase == that.toLowerCase
    case _ => false
  }

  override def hashCode = toLowerCase.hashCode

}

object EmailAddress {
  private val userPattern = """(?:[!#\$%&'\*\+\-/=\?\^_` \{\|\}~]|\w)(?:[!#\$%&'\*\+\-/=\?\^_` \{\|\}~]|\w|(?:\.(?=[^\.]))){0,63}""".r.pattern
  private val domainPattern = """(\S+\.[a-zA-Z]+)""".r.pattern
  private val splitPattern = "@".r.pattern

  private def split(address: String): (String, String) = maybeSplit(address) match {
    case None => throw new IllegalArgumentException("Not valid email address: " + address)
    case Some(ud) => ud
  }

  private def maybeSplit(address: String): Option[(String, String)] = splitPattern split address match {
    case Array(user, domain) => Some((user, domain))
    case _ => None
  }

  private def isValidLength(user: String, domain: String): Boolean = user.length + domain.length < 254
  private def isValidUser(user: String) = userPattern.matcher(user).matches()
  private def isValidDomain(domain: String) = domainPattern.matcher(domain).matches()

  def isValid(emailAddress: String): Boolean = maybeSplit(emailAddress) match {
    case None => false
    case Some((user, domain)) => isValidLength(user, domain) && isValidUser(user) && isValidDomain(domain)
  }
}
