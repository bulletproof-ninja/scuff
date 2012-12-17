package scuff.ddd

class DomainException(val reason: String, val parms: Any*) extends RuntimeException {
  def toString(fmt: scuff.PropertiesFormatter) = fmt(reason, parms)
} 

