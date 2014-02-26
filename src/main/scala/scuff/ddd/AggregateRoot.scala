package scuff.ddd

trait AggregateRoot {
  override final lazy val hashCode = id.hashCode ^ revision.map(_.asInstanceOf[Int]).getOrElse(17)
  override final def equals(obj: Any) = obj match {
    case that: AggregateRoot ⇒ this.id == that.id && this.revision == that.revision
    case _ ⇒ false
  }

  type EVT <: DomainEvent
  type ID
  /** Unique identifier. */
  def id: ID
  /** Current revision. */
  def revision: Option[Int]
  /** Events produced. */
  def events: List[_ <: EVT]
}
