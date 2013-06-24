package scuff.web.form

sealed trait Problem {
  def field: String
  def message: Option[String]
}

case class Required(field: String)(val message: Option[String] = None) extends Problem
object Required {
  def apply(field: String) = new Required(field)(None)
  def apply(field: String, message: String) = new Required(field)(Option(message))
}

case class Invalid(field: String)(cause: Option[Either[Exception, String]]) extends Problem {
  def message = cause.flatMap(_.right.toOption)
  def exception = cause.flatMap(_.left.toOption)
}
object Invalid {
  def apply(field: String) = new Invalid(field)(None)
  def apply(field: String, e: Exception) = new Invalid(field)(Option(e).map(Left(_)))
  def apply(field: String, message: String) = new Invalid(field)(Option(message).map(Right(_)))
}
