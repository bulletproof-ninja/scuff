package scuff

import java.security.Principal

case class UserPrincipal(userId: String, roles: Set[String]) extends Principal {
  def getName = userId
  override def toString = "%s[%s]".format(userId, roles.mkString(","))
  override def equals(any: Any) = any match {
    case that: Principal ⇒ this.getName == that.getName
    case _ ⇒ false
  }
  override def hashCode = userId.hashCode
}

