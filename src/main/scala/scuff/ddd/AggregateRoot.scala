package scuff.ddd

trait AggregateRoot {
  type EVT <: DomainEvent
  type ID
  def id: ID
  def revision: Option[Long]
  override final lazy val hashCode = id.hashCode ^ revision.map(_.asInstanceOf[Int]).getOrElse(17)
  override final def equals(obj: Any) = obj match {
    case that: AggregateRoot ⇒ this.id == that.id && this.revision == that.revision
    case _ ⇒ false
  }
  def newEvents: List[_ <: EVT]
}

