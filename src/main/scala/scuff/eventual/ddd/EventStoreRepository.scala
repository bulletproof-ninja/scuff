package scuff.eventual.ddd

import scuff.ddd._
import scuff.eventual._
import scala.concurrent._
import java.util.concurrent.TimeUnit
import scala.util.Failure
import scala.util.Success
import scuff.Threads.PiggyBack
import scala.annotation.implicitNotFound

/**
 * [[scuff.eventual.EventStore]]-based [[scuff.ddd.Repository]] implementation.
 */
@implicitNotFound("Cannot find implicit conversion of AggregateRoot#ID to generic EventStore id ESID")
abstract class EventStoreRepository[ESID, AR <: AggregateRoot <% CAT, CAT](implicit idConv: AR#ID ⇒ ESID) extends Repository[AR] {

  protected def clock: scuff.Clock = scuff.Clock.System

  protected val eventStore: EventStore[ESID, _ >: AR#EVT <: DomainEvent, CAT]

  def exists(id: AR#ID): Future[Boolean] = eventStore.exists(id)

  /** Domain state type. */
  protected type S
  private type TXN = eventStore.Transaction

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
  protected def newAggregateRoot(id: AR#ID, revision: Int, state: S, concurrentUpdates: List[_ <: AR#EVT]): AR

  private[this] val NoFuture = Future.successful(None)
  protected def loadSnapshot(id: AR#ID): Future[Option[(S, Int)]] = NoFuture
  protected def saveSnapshot(id: AR#ID, revision: Int, state: S) {}

  private[this] def loadRevision(id: AR#ID, revision: Int): Future[AR] = {
    eventStore.replayStreamTo(id, revision) { txns ⇒
      val stateBuilder = newStateMutator(None)
      var last: TXN = null
      txns.foreach { txn ⇒
        txn.events.foreach { e ⇒
          val evt = e.asInstanceOf[AR#EVT]
          stateBuilder(evt)
        }
        last = txn
      }
      last match {
        case null ⇒ throw new UnknownIdException(id)
        case lastTxn ⇒ newAggregateRoot(id, lastTxn.revision, stateBuilder.state, Nil)
      }
    }
  }

  /**
   * Notification for every aggregate loaded.
   * Can be used for statistics gathering,
   * or to determine snapshotting.
   */
  protected def onLoadNotification(id: AR#ID, revision: Int, category: CAT, timeMs: Long) {}

  /**
   * Optimistically assume any snapshot returned is the most current.
   * This will prevent replaying of any potential events after snapshot.
   */
  protected def assumeSnapshotCurrent = false

  private def loadLatest(id: AR#ID, allowAssumption: Boolean, lastSeenRevision: Int = Int.MaxValue): Future[AR] = {
      implicit def ec = PiggyBack
    val startTime = clock.now(TimeUnit.MILLISECONDS)
    val futureMutator = loadSnapshot(id).map {
      case Some((state, revision)) ⇒ (newStateMutator(Some(state)), Some(revision))
      case _ ⇒ (newStateMutator(None), None)
    }
    val futureState = futureMutator.flatMap {
      case (stateBuilder, snapshotRevision) ⇒
        val applyEventsAfter = snapshotRevision.getOrElse(-1)
          def handler(txns: Iterator[eventStore.Transaction]): Option[(S, Int, List[_ <: AR#EVT])] = {
            var concurrentUpdates: List[AR#EVT] = Nil
            var lastRev = -1
            txns.foreach { txn ⇒
              txn.events.foreach { e ⇒
                val evt = e.asInstanceOf[AR#EVT]
                if (txn.revision > applyEventsAfter) stateBuilder.apply(evt)
                if (txn.revision > lastSeenRevision) concurrentUpdates ::= evt
              }
              lastRev = txn.revision
            }
            if (lastRev == -1) {
              snapshotRevision.map(rev ⇒ (stateBuilder.state, rev, Nil))
            } else {
              Some(stateBuilder.state, lastRev, concurrentUpdates.reverse)
            }
          }
        snapshotRevision match {
          case Some(snapshotRevision) ⇒
            if (lastSeenRevision >= snapshotRevision && allowAssumption && assumeSnapshotCurrent) {
              val result = (stateBuilder.state, snapshotRevision, Nil)
              Future.successful(Some(result))
            } else {
              val replaySinceRev = math.min(snapshotRevision, lastSeenRevision)
              eventStore.replayStreamSince(id, replaySinceRev)(handler)
            }
          case _ ⇒ eventStore.replayStream(id)(handler)
        }
    }
    futureState.map {
      case Some((state, revision, concurrentUpdates)) ⇒
        val loadTime = clock.durationSince(startTime)(TimeUnit.MILLISECONDS)
        saveSnapshot(id, revision, state)
        val ar = newAggregateRoot(id, revision, state, concurrentUpdates)
        onLoadNotification(id, revision, ar, loadTime)
        ar
      case _ ⇒ throw new UnknownIdException(id)
    }
  }

  def load(id: AR#ID, revision: Option[Int]): Future[AR] = revision match {
    case Some(revision) ⇒ loadRevision(id, revision)
    case _ ⇒ loadLatest(id, true)
  }

  protected def insert(metadata: Map[String, String], ar: AR): Future[AR#ID] = {
    if (ar.revision.nonEmpty) {
      Future.failed(new IllegalStateException("Cannot insert. %s already has revision %d".format(ar.id, ar.revision.get)))
    } else if (ar.events.isEmpty) {
      Future.failed(new IllegalStateException("Cannot insert. %s has produced no events.".format(ar.id)))
    } else {
        implicit def ec = PiggyBack
      eventStore.record(ar, ar.id, 0, ar.events, metadata).map(_ ⇒ ar.id).recoverWith {
        case _: DuplicateRevisionException ⇒ Future.failed(new DuplicateIdException(ar.id))
      }
    }
  }

  private[this] def recordUpdate(ar: AR, metadata: Map[String, String]): Future[Int] = {
    val newRevision = ar.revision.getOrElse(-1) + 1
    eventStore.record(ar, ar.id, newRevision, ar.events, metadata).map(txn ⇒ txn.revision)(PiggyBack)
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

  private def loadAndUpdate(id: AR#ID, basedOnRevision: Int, metadata: Map[String, String], doAssume: Boolean, handler: AR ⇒ Unit): Future[Int] = {
      implicit def ec = PiggyBack
    loadLatest(id, doAssume, basedOnRevision).flatMap { ar ⇒
      val t = handler.apply(ar)
      if (ar.events.isEmpty) {
        Future.successful(ar.revision.get)
      } else {
        recordUpdate(ar, metadata).recoverWith {
          case e: DuplicateRevisionException ⇒
            onConcurrentUpdateCollision(id, e.revision, ar)
            loadAndUpdate(id, basedOnRevision, metadata, false, handler)
        }
      }
    }
  }
  protected def update(id: AR#ID, basedOnRevision: Int, metadata: Map[String, String])(updateBlock: AR ⇒ Unit): Future[Int] = try {
    loadAndUpdate(id, basedOnRevision, metadata, true, updateBlock)
  } catch {
    case e: Exception ⇒ Future.failed(e)
  }

}
