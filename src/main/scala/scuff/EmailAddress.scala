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

  require(user.length + domain.length < 254, "Address exceeds 254 characters: " + user)
  require(userPattern.matcher(user).matches, "Invalid user name: " + user)
  require(domainPattern.matcher(domain).matches, "Invalid domain name: " + domain)

  override val toString = user + "@" + domain
  private val lowerCase = toString.toLowerCase

  def compareTo(that: EmailAddress) = this.lowerCase compareTo that.lowerCase

  override def equals(any: Any) = any match {
    case that: EmailAddress ⇒ this.lowerCase == that.lowerCase
    case _ ⇒ false
  }

  override def hashCode = lowerCase.hashCode

}

import java.util.regex._
private object EmailAddress {

  private val userPattern = Pattern compile """(?:[!#\$%&'\*\+\-/=\?\^_` \{\|\}~]|\w)(?:[!#\$%&'\*\+\-/=\?\^_` \{\|\}~]|\w|(?:\.(?=[^\.]))){0,63}"""
  private val domainPattern = Pattern compile """(\S+\.[a-zA-Z]+)"""
  private val splitPattern = Pattern compile "@"

  private def split(address: String) = {
    val parts = splitPattern split address
    if (parts.length != 2) throw new IllegalArgumentException("Not valid email address: " + address)
    (parts(0), parts(1))
  }

}
