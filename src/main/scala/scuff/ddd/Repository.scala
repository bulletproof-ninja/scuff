package scuff.ddd

/**
  * Aggregate root repository.
  */
trait Repository[AR <: AggregateRoot] {

  protected val IgnoreRevision = Long.MaxValue

  /**
    * Load aggregate. Only for reading the aggregate.
    * Any modifications cannot be saved.
    * @param id The instance ID
    * @param revision Load a specific revision, or None for most current
    * @return The requested revision
    */
  @throws(classOf[UnknownIdException])
  def load(id: AR#ID, revision: Option[Long] = None): AR
  final def load(id: (AR#ID, Long)): AR = load(id._1, Some(id._2))

  /**
    * Update aggregate. Changes are committed and any revision conflict is
    * automatically retried. To back out of the transaction, throw an exception.
    * NOTICE: Anything done in this block must be idempotent, due to potential
    * automatic retry on concurrent updates.
    * @param id The aggregate ID
    * @param basedOnRevision Revision, which update will be based on
    * @param updateBlock The transaction code block. This may be executed multiple times if concurrent updates occur
    * @return The last committed revision
    */
  @throws(classOf[UnknownIdException])
  def update(id: AR#ID, basedOnRevision: Long)(updateBlock: AR ⇒ Unit): Long
  final def update(id: (AR#ID, Long))(updateBlock: AR ⇒ Unit): Long = update(id._1, id._2)(updateBlock)

  /**
    * Insert new aggregate root and publish committed events.
    * <p>NOTICE: The aggregate root instance should not be
    * used after it's been saved. Will throw [[DuplicateIdException]] if
    * aggregate has already been saved.
    * @param ar Aggregate root to save
    */
  @throws(classOf[DuplicateIdException])
  def insert(ar: AR)
}

class UnknownIdException(val id: Any) extends RuntimeException("Unknown aggregate: " + id)
class DuplicateIdException(val id: Any) extends RuntimeException("Aggregate is already saved: " + id)
