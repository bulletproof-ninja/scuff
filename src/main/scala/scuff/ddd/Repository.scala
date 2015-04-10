package scuff.ddd

import language.implicitConversions
import scala.concurrent.Future
import scala.util.control.NonFatal

import scuff.concurrent.Threads

/**
 * Aggregate root repository.
 */
trait Repository[AR <: AggregateRoot] {

  def exists(id: AR#ID): Future[Boolean]

  /**
   * Load aggregate. Only for reading the aggregate.
   * Any modifications cannot be saved.
   * @param id The instance ID
   * @param revision Load a specific revision, or None for most latest (default)
   * @return The specific or latest revision of AR or [[scuff.ddd.UnknownIdException]]
   */
  def load(id: AR#ID, revision: Option[Int] = None): Future[AR]
  final def load(id: (AR#ID, Int)): Future[AR] = load(id._1, Some(id._2))
  final def load(id: AR#ID, revision: Int): Future[AR] = load(id, Some(revision))

  private def checkInvariants(ar: AR) = {
    if (ar.events.nonEmpty) try {
      ar.checkInvariants()
    } catch {
      case e: Exception => throw new InvariantFailure(ar.id, ar.events, e)
    }

  }
  private def updateThenCheckInvariants[T](updateThunk: AR => Future[T])(ar: AR): Future[T] = {
    updateThunk(ar).map { t =>
      checkInvariants(ar)
      t
    }(Threads.PiggyBack)
  }

  @inline
  private implicit def intRevision(rev: Option[Int]): Int = rev.getOrElse(Int.MaxValue)

  /**
   * Update aggregate. Changes are committed and potential revision conflicts are
   * automatically retried. To back out of the transaction, throw an exception.
   * NOTICE: Anything done in this block must be thread-safe and idempotent,
   * due to the automatic retry on concurrent updates.
   * @param id The aggregate ID
   * @param basedOnRevision Revision, which update will be based on
   * @param metadata The update metadata
   * @param updateThunk The transaction update thunk. This may be executed multiple times if concurrent updates occur
   */
  final def update[T](id: AR#ID, basedOnRevision: Int, metadata: Map[String, String])(updateThunk: AR => Future[T]): Future[Updated[T]] =
    update(metadata, id, basedOnRevision, updateThenCheckInvariants(updateThunk))

  /**
   * Update aggregate. Changes are committed and potential revision conflicts are
   * automatically retried. To back out of the transaction, throw an exception.
   * NOTICE: Anything done in this block must be thread-safe and idempotent,
   * due to the automatic retry on concurrent updates.
   * @param id The aggregate ID
   * @param basedOnRevision Revision, which update will be based on
   * @param updateThunk The transaction update thunk. This may be executed multiple times if concurrent updates occur
   */
  final def update[T](id: AR#ID, basedOnRevision: Int)(updateThunk: AR => Future[T]): Future[Updated[T]] =
    update(Map.empty, id, basedOnRevision, updateThenCheckInvariants(updateThunk))

  final def update[T](id: AR#ID, basedOnRevision: Option[Int], metadata: Map[String, String])(updateThunk: AR => Future[T]): Future[Updated[T]] =
    update(metadata, id, basedOnRevision, updateThenCheckInvariants(updateThunk))

  final def update[T](id: AR#ID, basedOnRevision: Option[Int])(updateThunk: AR => Future[T]): Future[Updated[T]] =
    update(Map.empty, id, basedOnRevision, updateThenCheckInvariants(updateThunk))

  /**
   * Update implementation.
   * @param metadata The update metadata.
   * @param id The AR id
   * @param basedOnRevision The revision that this update is based on. Will be `Int.MaxValue` if unknown or irrelevant.
   * @param updateThunk The block of coding invoking changes to the AR
   * @return `Future` indicating successful update or failure. Should fail with [[scuff.ddd.UnknownIdException]] if AR id is unknown.
   */
  protected def update[T](metadata: Map[String, String], id: AR#ID, basedOnRevision: Int, updateThunk: AR => Future[T]): Future[Updated[T]]

  /**
   * Insert new aggregate root and publish committed events.
   * <p>NOTICE: No commands should be applied to the aggregate instance
   * after it's been saved, as it cannot be persisted again. Instead `update`.
   * @param aggr Aggregate root to save
   * @return `Future` indicating successful insertion or failure. Fails with [[scuff.ddd.DuplicateIdException]] if AR id already exists,
   * and [[java.lang.IllegalStateException]] if `revision` is present (e.g. `Some(revision)`).
   */
  final def insert(aggr: AR, metadata: Map[String, String] = Map.empty): Future[AR#ID] = {
    try {
      checkInvariants(aggr)
      insert(metadata, aggr)
    } catch {
      case NonFatal(e) => Future failed e
    }
  }

  /**
   * Insert implementation.
   * @param metadata The insert metadata
   * @param aggr The AR to insert
   * @return `Future` indicating successful insertion or failure. Should fail with [[scuff.ddd.DuplicateIdException]] if AR id already exists,
   * and [[java.lang.IllegalStateException]] if `revision` is present (not `None`).
   */
  protected def insert(metadata: Map[String, String], aggr: AR): Future[AR#ID]

  private[this] final val EventPrefix = s"${compat.Platform.EOL}\t * "
  final class InvariantFailure(aggr: AR#ID, events: List[DomainEvent], cause: Exception)
    extends RuntimeException(s"""Aggregate $aggr invariant failure: "${cause.getMessage}" with events:${events.mkString(EventPrefix, EventPrefix, "")}""", cause)

  final class Updated[T](val revision: Int, val output: T)

}

case class UnknownIdException(id: Any) extends RuntimeException("Unknown aggregate: " + id)
case class DuplicateIdException(id: Any) extends RuntimeException("Aggregate already exists: " + id)
