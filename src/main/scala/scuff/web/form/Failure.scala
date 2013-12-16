package scuff.web.form

case class Failure(field: String, failureType: FailureType, message: Option[String] = None)

sealed trait FailureType
object FailureType {
  case object Missing extends FailureType
  case object Syntax extends FailureType
  case object Semantic extends FailureType
}
