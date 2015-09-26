package scuff.ddd

import scala.collection.immutable.Seq
import scala.concurrent.{ ExecutionContext, Future }
import scala.language.implicitConversions
import scala.util.{ Failure, Success, Try }
import scala.util.control.NonFatal
import scuff.concurrent.Threads.PiggyBack
import scuff.concurrent.Threads

/**
 * Aggregate root repository.
 */
trait Repository[AR, ID] {

  /**
   * Does aggregate exist?
   * @return Current revision, if exists
   */
  def exists(id: ID): Future[Option[Int]]

  /**
   * Load aggregate. Only for reading.
   * Modifications cannot be saved.
   * @param id The instance ID
   * @return The latest revision of AR or [[scuff.ddd.UnknownIdException]]
   */
  def load(id: ID): Future[(AR, Int)]
  /**
   * Update aggregate.
   * @param id The instance id
   * @param expectedRevision The revision that this update is based on. `None` if unknown or irrelevant.
   * @param metadata Optional metadata.
   * @param updateThunk The code block responsible for updating. If no events are returned, the repository will
   * @return New revision, or [[scuff.ddd.UnknownIdException]] if unknown id.
   */
  def update(id: ID, expectedRevision: Option[Int], metadata: Map[String, String] = Map.empty)(updateThunk: (AR, Int) => Future[AR]): Future[Int]

  final def update(id: ID, expectedRevision: Int, metadata: Map[String, String])(updateThunk: (AR, Int) => Future[AR]): Future[Int] =
    update(id, Some(expectedRevision), metadata)(updateThunk)
  final def update(id: ID, expectedRevision: Int)(updateThunk: (AR, Int) => Future[AR]): Future[Int] =
    update(id, Some(expectedRevision))(updateThunk)

  /**
   * Insert new aggregate. Must, by definition, be given revision `0`.
   * @param ar The instance to insert
   * @param events The events.
   * @param metadata Optional metadata.
   * @return The revision (always `0`) if successful, or [[scuff.ddd.DuplicateIdException]] if id already exists
   */
  def insert(id: ID, ar: AR, metadata: Map[String, String] = Map.empty): Future[Int]
}

abstract class EventCentricRepository[AR, ID, EVT](impl: Repository[AR, ID])(implicit publishCtx: ExecutionContext)
    extends Repository[(AR, Seq[EVT]), ID] {
  type ARwithEvents = (AR, Seq[EVT])
  protected def publish(id: ID, revision: Int, events: Seq[EVT], metadata: Map[String, String])

  private def publishEvents(id: ID, revision: Int, events: Seq[EVT], metadata: Map[String, String]) {
    if (events.nonEmpty) try publish(id, revision, events, metadata) catch {
      case NonFatal(e) => publishCtx.reportFailure(e)
    }
  }

  def exists(id: ID): Future[Option[Int]] = impl.exists(id)
  def load(id: ID): Future[(ARwithEvents, Int)] = impl.load(id).map {
    case (ar, rev) => ar -> Nil -> rev
  }
  def update(id: ID, expectedRevision: Option[Int], metadata: Map[String, String] = Map.empty)(updateThunk: (ARwithEvents, Int) => Future[ARwithEvents]): Future[Int] = {
    @volatile var updatedAR: Future[ARwithEvents] = null
    val updatedRev = impl.update(id, expectedRevision, metadata) {
      case (ar, rev) =>
        updatedAR = updateThunk(ar -> Nil, rev)
        updatedAR.map(_._1)
    }
    for (rev <- updatedRev; (ar, events) <- updatedAR) {
      publishEvents(id, rev, events, metadata)
    }
    updatedRev
  }

  def insert(id: ID, arWithEvents: ARwithEvents, metadata: Map[String, String] = Map.empty): Future[Int] = {
    val (ar, events) = arWithEvents
    val inserted = impl.insert(id, ar, metadata)
    for (rev <- inserted) {
      publishEvents(id, rev, events, metadata)
    }
    inserted
  }

}

final case class UnknownIdException(id: Any) extends RuntimeException(s"Unknown id: $id")
final case class DuplicateIdException(id: Any) extends RuntimeException(s"Id already exists: $id")
