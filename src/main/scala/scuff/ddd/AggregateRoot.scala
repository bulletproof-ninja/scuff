package scuff.ddd

trait AggregateRoot {
  override final lazy val hashCode = id.hashCode ^ revision.getOrElse(-1)
  override final def equals(obj: Any) = obj match {
    case that: AggregateRoot => this.id == that.id && this.revision == that.revision
    case _ => false
  }

  /** ID type. */
  type ID
  /** Unique identifier. */
  def id: ID
  /** Domain event type. */
  type EVT <: DomainEvent
  /** Current revision. */
  def revision: Option[Int]
  /** Events produced. */
  def events: List[_ <: EVT]
  /**
   * Check invariants of this aggregate and
   * throw an exception on failure.
   */
  def checkInvariants(): Unit
}
