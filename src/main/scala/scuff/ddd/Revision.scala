package scuff.ddd

/** Expected revision. */
sealed abstract class Revision {
  private[ddd] def validate(actual: Int): Unit
  def value: Option[Int]
}
object Revision {

  def apply(expected: Int): Revision = apply(expected, true)
  def apply(expected: Option[Int]): Revision = apply(expected, true)
  def apply(expected: Int, allowMerge: Boolean): Revision =
    if (allowMerge) new AllowMerge(expected)
    else new Exactly(expected)
  def apply(expected: Option[Int], allowMerge: Boolean): Revision =
    expected match {
      case Some(expected) => apply(expected, allowMerge)
      case _ => Latest
    }

  /** Must be exact, i.e. do not allow merge, else fail. */
  case class Exactly(expected: Int) extends Revision {
    def value = Some(expected)
    private[ddd] def validate(actual: Int): Unit = {
      if (expected != actual) {
        throw new Mismatch(expected, actual)
      }
    }
  }
  /** For updates, allow merging, if possible. */
  case class AllowMerge(expected: Int) extends Revision {
    def value = Some(expected)
    private[ddd] def validate(actual: Int): Unit = {
      if (expected > actual) {
        throw new Mismatch(expected, actual)
      }
    }
  }
  /** Revision not known, or is irrelevant, use latest actual. */
  case object Latest extends Revision {
    def value = None
    private[ddd] def validate(actual: Int): Unit = ()
  }

  /** Revision mismatch exception. */
  final case class Mismatch(expected: Int, actual: Int)
    extends RuntimeException(s"Expected $expected, was $actual")
    with scala.util.control.NoStackTrace

}
