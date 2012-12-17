package scuff.ddd

trait AggregateRoot {
  type EVT <: DomainEvent
  type ID
  def id: ID
  def committedRevision: Option[Long]
  override final lazy val hashCode = id.hashCode ^ committedRevision.map(_.asInstanceOf[Int]).getOrElse(17)
  override final def equals(obj: Any) = obj match {
    case that: AggregateRoot ⇒ this.id == that.id && this.committedRevision == that.committedRevision
    case _ ⇒ false
  }
  def uncommittedEvents: List[_ <: EVT]
}

