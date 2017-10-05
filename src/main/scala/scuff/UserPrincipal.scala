package scuff

import java.security.Principal

case class UserPrincipal(userId: String, roles: Set[String]) extends Principal {
  def getName = userId
  override def toString = s"$userId[${roles.mkString(",")}]"
  override def equals(that: Any) = that match {
    case that: Principal => (this eq that) || this.getName == that.getName
    case _ => false
  }
  override def hashCode = userId.hashCode
}
