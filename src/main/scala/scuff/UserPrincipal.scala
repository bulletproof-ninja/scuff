package scuff

import java.security.Principal

case class UserPrincipal(login: String, roles: Set[String]) extends Principal {
  def getName = login
  override def toString = "%s[%s]".format(login, roles.mkString(","))
  override def equals(any: Any) = any match {
    case that: Principal ⇒ this.getName == that.getName
    case _ ⇒ false
  }
  override def hashCode = login.hashCode
}

