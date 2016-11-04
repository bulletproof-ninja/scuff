package scuff.ddd

class DomainException(val reason: String, val parms: Any*) extends RuntimeException(reason) {
  def toString(fmt: scuff.L10nPropFormatter) = fmt(reason, parms)
  override def toString = reason
}
