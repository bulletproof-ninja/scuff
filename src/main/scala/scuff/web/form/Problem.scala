package scuff.web.form

case class Problem(field: String, problemType: ProblemType, message: Option[String] = None)

sealed trait ProblemType
object ProblemType {
  case object Missing extends ProblemType
  case object Syntax extends ProblemType
  case object Semantic extends ProblemType
}
