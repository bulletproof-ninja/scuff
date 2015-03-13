package scuff.eventual.ddd

import scuff.ddd._
import scuff.eventual._
import scala.concurrent._
import java.util.concurrent.TimeUnit
import scala.util.Failure
import scala.util.Success
import scuff.Threads.PiggyBack
import scala.annotation.implicitNotFound
import scala.util.control.NonFatal

/**
 * [[scuff.eventual.EventStore]]-based [[scuff.ddd.Repository]] implementation.
 */
@implicitNotFound("Cannot find implicit conversion of AggregateRoot#ID to generic EventStore id ESID")
abstract class EventStoreRepository[ESID, AR <: AggregateRoot <% CAT, CAT](implicit idConv: AR#ID => ESID) extends Repository[AR] {

  protected def loadTimer: scuff.Clock = scuff.Clock.System

  protected val eventStore: EventStore[ESID, _ >: AR#EVT <: DomainEvent, CAT]

  def exists(id: AR#ID): Future[Boolean] = eventStore.exists(id)

  /** Domain state type. */
  protected type S
  private type TXN = eventStore.Transaction
  protected case class Snapshot(state: S, revision: Int, timestamp: Long)

  protected def newTimestamp(causalTimestamp: Long): Long

  /**
   * Create new state mutator for rebuilding state.
   */
  protected def newStateMutator(snapshotState: Option[S]): StateMutator[AR#EVT, S]

  /**
   * Create an aggregate root instance, using the provided data and revision.
   * @param state The aggregate root state data
   * @param concurrentUpdate These are events that the client is unaware of and therefore it
   * must be determined if there's a conflict with the command given. This allows for fine grained
   * control over concurrent updates if needed, or can either be ignored (for last-man-wins updates) or
   * always prevent update (first-man-wins) for a conservative update strategy.
   */
  protected def newAggregateRoot(id: AR#ID, revision: Int, state: S, concurrentUpdates: List[AR#EVT]): AR

  private[this] val NoSnapshot = Future.successful(None)
  protected def loadSnapshot(id: AR#ID): Future[Option[Snapshot]] = NoSnapshot
  protected def saveSnapshot(id: AR#ID, snapshot: Snapshot) {}

  private[this] def loadRevision(id: AR#ID, revision: Int): Future[AR] = {
    eventStore.replayStreamTo(id, revision) { txns =>
      val stateBuilder = newStateMutator(None)
      var last: TXN = null
      txns.foreach { txn =>
        txn.events.foreach { e =>
          val evt = e.asInstanceOf[AR#EVT]
          stateBuilder(evt)
        }
        last = txn
      }
      last match {
        case null => throw new UnknownIdException(id)
        case lastTxn => newAggregateRoot(id, lastTxn.revision, stateBuilder.state, Nil)
      }
    }
  }

  /**
   * Notification for every aggregate loaded.
   * Can be used for statistics gathering,
   * or to determine snapshotting.
   * @param id The AR id
   * @param revision The AR revision
   * @param category The AR category
   * @param duration The load time duration, in [[clock]] precision.
   */
  protected def onLoadNotification(id: AR#ID, revision: Int, category: CAT, duration: Long) {}

  /**
   * Optimistically assume any snapshot returned is the most current.
   * This will prevent replaying of any potential events after snapshot.
   */
  protected def assumeSnapshotCurrent = false

  private def buildCurrentSnapshot(snapshot: Option[Snapshot], stateMutator: StateMutator[AR#EVT, S], lastSeenRevision: Int)(txns: Iterator[eventStore.Transaction]): Option[(Snapshot, List[_ <: AR#EVT])] = {
    val applyEventsAfter = snapshot.map(_.revision) getOrElse -1
    val (concurrentUpdates, lastTxn) =
      txns.foldLeft((List.empty[AR#EVT], None: Option[eventStore.Transaction])) {
        case (acc, txn) =>
          txn.events.foldLeft(acc) {
            case ((concurrentUpdates, _), e) =>
              val evt = e.asInstanceOf[AR#EVT]
              if (txn.revision > applyEventsAfter) stateMutator.apply(evt)
              (if (txn.revision > lastSeenRevision) evt :: concurrentUpdates else concurrentUpdates) -> Some(txn)
          }
      }
    lastTxn match {
      case None => snapshot.map(_ -> Nil)
      case Some(lastTxn) => Some(Snapshot(stateMutator.state, lastTxn.revision, lastTxn.timestamp), concurrentUpdates.reverse)
    }
  }

  private def loadLatest(id: AR#ID, snapshot: Future[Option[Snapshot]], assumeSnapshotCurrent: Boolean, lastSeenRevision: Int = Int.MaxValue): Future[(AR, Snapshot)] = {
      implicit def ec = PiggyBack
    val startTime = loadTimer.now()
    //    val futureMutator = snapshot.recover { case NonFatal(_) => None }.map { snapshot =>
    //      newStateMutator(snapshot.map(_.state)) -> snapshot
    //    }
    val futureState: Future[Option[(Snapshot, List[_ <: AR#EVT])]] = snapshot.flatMap { maybeSnapshot =>
      val stateMutator = newStateMutator(maybeSnapshot.map(_.state))
      maybeSnapshot match {
        case Some(snapshot) =>
          if (lastSeenRevision >= snapshot.revision && assumeSnapshotCurrent) {
            Future successful Some(snapshot, Nil)
          } else {
            val replayAfterRev = snapshot.revision min lastSeenRevision
            eventStore.replayStreamAfter(id, replayAfterRev)(buildCurrentSnapshot(maybeSnapshot, stateMutator, lastSeenRevision))
          }
        case _ => eventStore.replayStream(id)(buildCurrentSnapshot(maybeSnapshot, stateMutator, lastSeenRevision))
      }
    }
    futureState.map {
      case Some((snapshot, concurrentUpdates)) =>
        val loadTime = loadTimer.durationSince(startTime)
        val ar = newAggregateRoot(id, snapshot.revision, snapshot.state, concurrentUpdates)
        onLoadNotification(id, snapshot.revision, ar, loadTime)
        ar -> snapshot
      case _ => throw new UnknownIdException(id)
    }
  }

  def load(id: AR#ID, revision: Option[Int]): Future[AR] = revision match {
    case Some(revision) => loadRevision(id, revision)
    case _ => loadLatest(id, NoSnapshot, false).map(_._1)(PiggyBack)
  }

  protected def insert(metadata: Map[String, String], ar: AR, causalTimestamp: Long): Future[AR#ID] = {
    if (ar.revision.nonEmpty) {
      Future.failed(new IllegalStateException("Cannot insert. %s already has revision %d".format(ar.id, ar.revision.get)))
    } else if (ar.events.isEmpty) {
      Future.failed(new IllegalStateException("Cannot insert. %s has produced no events.".format(ar.id)))
    } else {
        implicit def ec = PiggyBack
      val timestamp = newTimestamp(causalTimestamp)
      eventStore.record(timestamp, ar, ar.id, 0, ar.events, metadata).map(_ => ar.id).recoverWith {
        case _: eventStore.DuplicateRevisionException => Future.failed(new DuplicateIdException(ar.id))
      }
    }
  }

  private[this] def recordUpdate(ar: AR, metadata: Map[String, String], timestamp: Long): Future[Int] = {
    val newRevision = ar.revision.getOrElse(-1) + 1
    eventStore.record(timestamp, ar, ar.id, newRevision, ar.events, metadata).map(txn => txn.revision)(PiggyBack)
  }

  /**
   * Notification on every concurrent update collision.
   * This happens when two aggregates are attempted to
   * be updated concurrently. Should be exceedingly rare,
   * unless there is unusually high contention on a
   * specific aggregate.
   * Can be used for monitoring and reporting.
   */
  protected def onConcurrentUpdateCollision(id: AR#ID, revision: Int, category: CAT) {}

  private def loadAndUpdate[T](
    id: AR#ID, basedOnRevision: Int, metadata: Map[String, String],
    causalTimestamp: Long,
    snapshot: Future[Option[Snapshot]], assumeSnapshotCurrent: Boolean,
    handler: AR => Future[T]): Future[Updated[(T, Snapshot)]] = {
      implicit def ec = PiggyBack
    loadLatest(id, snapshot, assumeSnapshotCurrent, basedOnRevision).flatMap {
      case (ar, snapshot) =>
        handler.apply(ar).flatMap { t =>
          if (ar.events.isEmpty) {
            Future successful new Updated(ar.revision.get, t -> snapshot)
          } else {
            val timestamp = newTimestamp(snapshot.timestamp max causalTimestamp)
            recordUpdate(ar, metadata, timestamp).map(rev => new Updated(rev, t -> snapshot)).recoverWith {
              case e: eventStore.DuplicateRevisionException =>
                val stateMutator = newStateMutator(Some(snapshot.state))
                e.conflictingTransaction.events.foreach { evt =>
                  stateMutator(evt.asInstanceOf[AR#EVT])
                }
                val latestSnapshot = Future successful Some(Snapshot(stateMutator.state, e.conflictingTransaction.revision, e.conflictingTransaction.timestamp))
                onConcurrentUpdateCollision(id, e.conflictingTransaction.revision, ar)
                loadAndUpdate(id, basedOnRevision, metadata, timestamp, latestSnapshot, true, handler)
            }
          }
        }
    }
  }
  protected def update[T](id: AR#ID, basedOnRevision: Int, metadata: Map[String, String], causalTimestamp: Long)(updateBlock: AR => Future[T]): Future[Updated[T]] = try {
    loadAndUpdate(id, basedOnRevision, metadata, causalTimestamp, loadSnapshot(id), assumeSnapshotCurrent, updateBlock).map { updated =>
      saveSnapshot(id, updated.output._2)
      new Updated(updated.revision, updated.output._1)
    }(PiggyBack)
  } catch {
    case e: Exception => Future failed e
  }

}
