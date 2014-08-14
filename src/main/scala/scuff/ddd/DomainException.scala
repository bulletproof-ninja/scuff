package scuff.ddd

class DomainException(val reason: String, val parms: Any*) extends RuntimeException(reason) {
  def toString(fmt: scuff.PropertiesFormatter) = fmt(reason, parms)
  override def toString = s"Domain failure: $reason"
} 

