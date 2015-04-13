package scuff.ddd

/**
 * Aggregate root trait.
 */
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
  def events: List[EVT]
  /**
   * Check invariants of this aggregate and
   * throw an exception on failure.
   */
  def checkInvariants(): Unit
}

/**
 * Type-class for generic aggregate root.
 */
trait AggrRoot[AR] {
  type ID
  type EVT
  def domainEvt: DomainEvt[EVT]
  def id(ar: AR): ID
  def revision(ar: AR): Option[Int]
  def events(ar: AR): List[EVT]
  def checkInvariants(ar: AR): Unit
}

/**
 * Type-class instance for aggregate roots extending [[scuff.ddd.AggregateRoot]].
 */
class DefaultAggrRoot[AR <: AggregateRoot](implicit val domainEvt: DomainEvt[AR#EVT]) extends AggrRoot[AR] {
  type ID = AR#ID
  type EVT = AR#EVT
  def id(ar: AR) = ar.id
  def revision(ar: AR) = ar.revision
  def events(ar: AR) = ar.events
  def checkInvariants(ar: AR) = ar.checkInvariants()
}
