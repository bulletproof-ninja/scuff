package scuff.ddd

import scala.util.control.NoStackTrace

class DomainException(val reason: String, val parms: Any*) extends RuntimeException with NoStackTrace {
  def toString(fmt: scuff.PropertiesFormatter) = fmt(reason, parms)
  override def toString = s"Domain failure: $reason"
} 

