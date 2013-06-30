package scuff.ddd

import scala.concurrent.Future

/**
 * Aggregate root repository.
 */
trait Repository[AR <: AggregateRoot] {

  /**
   * Load aggregate. Only for reading the aggregate.
   * Any modifications cannot be saved.
   * @param id The instance ID
   * @param revision Load a specific revision, or None for most current
   * @return The requested revision or [[UnknownIdException]]
   */
  def load(id: AR#ID, revision: Option[Long] = None): Future[AR]
  final def load(id: (AR#ID, Long)): Future[AR] = load(id._1, Some(id._2))
  final def load(id: AR#ID, revision: Long): Future[AR] = load(id, Some(revision))

  /**
   * Update aggregate. Changes are committed and any revision conflict is
   * automatically retried. To back out of the transaction, throw an exception.
   * NOTICE: Anything done in this block must be idempotent, due to potential
   * automatic retry on concurrent updates.
   * @param id The aggregate ID
   * @param basedOnRevision Revision, which update will be based on
   * @param updateBlock The transaction code block. This may be executed multiple times if concurrent updates occur
   * @return The last committed revision or [[UnknownIdException]]
   */
  def update(id: AR#ID, basedOnRevision: Long, metadata: Map[String, String])(updateBlock: AR ⇒ Unit): Future[Long]
  final def update(id: AR#ID, basedOnRevision: Long)(updateBlock: AR ⇒ Unit): Future[Long] = update(id, basedOnRevision, Map.empty)(updateBlock)
  final def update(id: (AR#ID, Long))(updateBlock: AR ⇒ Unit): Future[Long] = update(id._1, id._2, Map.empty)(updateBlock)
  final def update(id: (AR#ID, Long), metadata: Map[String, String])(updateBlock: AR ⇒ Unit): Future[Long] = update(id._1, id._2, metadata)(updateBlock)

  /**
   * Insert new aggregate root and publish committed events.
   * <p>NOTICE: The aggregate root instance should not be
   * used after it's been saved, as it can neither be re-inserted,
   * nor further updated.
   * Will throw [[IllegalStateException]] if aggregate already has a
   * revision number.
   * @param getAR Aggregate root to save
   * @return Potential [[DuplicateIdException]]
   */
  final def insert(getAR: ⇒ AR): Future[Unit] = insert(Map.empty[String, String])(getAR)
  def insert(metadata: Map[String, String])(getAR: ⇒ AR): Future[Unit]
}

class UnknownIdException(val id: Any) extends RuntimeException("Unknown aggregate: " + id)
class DuplicateIdException(val id: Any) extends RuntimeException("Aggregate is already saved: " + id)
