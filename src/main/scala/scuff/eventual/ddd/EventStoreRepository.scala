package scuff.eventual.ddd

import scuff.ddd._
import scuff.eventual._
import scala.concurrent._
import scuff.Threads.PiggyBack
import java.util.concurrent.TimeUnit
import scala.util.Failure
import scala.util.Success
import scuff.Threads

/**
 * [[scuff.eventual.EventStore]]-based [[scuff.ddd.Repository]] implementation.
 */
abstract class EventStoreRepository[ESID, AR <: AggregateRoot <% CAT, CAT](implicit idConv: AR#ID ⇒ ESID) extends Repository[AR] {

  implicit protected def exeCtx: ExecutionContext = Threads.PiggyBack

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

  private def loadLatest(id: AR#ID, doAssume: Boolean, lastSeenRevision: Int = Int.MaxValue): Future[AR] = {
    implicit val Millis = TimeUnit.MILLISECONDS
    val startTime = clock.now
    val futureMutator = loadSnapshot(id).map { snapshot ⇒
      snapshot match {
        case Some((state, revision)) ⇒ (newStateMutator(Some(state)), Some(revision))
        case None ⇒ (newStateMutator(None), None)
      }
    }
    val futureState = futureMutator.flatMap {
      case (stateBuilder, snapshotRevision) ⇒
        val applyEventsAfter = snapshotRevision.getOrElse(-1)
          def handler(txns: Iterator[eventStore.Transaction]): Option[(S, Int, List[_ <: AR#EVT])] = {
            var concurrentUpdates: List[AR#EVT] = Nil
            var last: TXN = null
            txns.foreach { txn ⇒
              txn.events.foreach { e ⇒
                val evt = e.asInstanceOf[AR#EVT]
                if (txn.revision > applyEventsAfter) stateBuilder.apply(evt)
                if (txn.revision > lastSeenRevision) concurrentUpdates ::= evt
              }
              last = txn
            }
            last match {
              case null ⇒ None
              case last ⇒ Some(stateBuilder.state, last.revision, concurrentUpdates.reverse)
            }
          }
        snapshotRevision match {
          case None ⇒ eventStore.replayStream(id)(handler)
          case Some(snapshotRevision) ⇒
            if (doAssume && assumeSnapshotCurrent) {
              Future.successful(Some(stateBuilder.state, snapshotRevision, Nil))
            } else {
              val replaySinceRev = math.min(snapshotRevision, lastSeenRevision)
              eventStore.replayStreamSince(id, replaySinceRev)(handler)
            }
        }
    }
    futureState.map {
      case None ⇒ throw new UnknownIdException(id)
      case Some((state, revision, concurrentUpdates)) ⇒
        val loadTime = clock.durationSince(startTime)
        saveSnapshot(id, revision, state)
        val ar = newAggregateRoot(id, revision, state, concurrentUpdates)
        onLoadNotification(id, revision, ar, loadTime)
        ar
    }
  }

  def load(id: AR#ID, revision: Option[Int]): Future[AR] = revision match {
    case None ⇒ loadLatest(id, true)
    case Some(revision) ⇒ loadRevision(id, revision)
  }

  def insert(ar: AR)(implicit metadata: Map[String, String] = Map.empty): Future[AR] = {
    if (ar.revision.nonEmpty) {
      Future.failed(new IllegalStateException("Cannot insert. %s already has revision %d".format(ar.id, ar.revision.get)))
    } else if (ar.events.isEmpty) {
      Future.failed(new IllegalStateException("Cannot insert. %s has produced no events.".format(ar.id)))
    } else {
      eventStore.record(ar, ar.id, 0, ar.events, metadata).map(_ ⇒ ar).recoverWith {
        case _: DuplicateRevisionException ⇒ Future.failed(new DuplicateIdException(ar.id))
      }
    }
  }

  private[this] def recordUpdate(ar: AR, metadata: Map[String, String]): Future[Int] = {
    val newRevision = ar.revision.getOrElse(-1) + 1
    eventStore.record(ar, ar.id, newRevision, ar.events, metadata).map(_ ⇒ newRevision)
  }

  private def loadAndUpdate[T](id: AR#ID, basedOnRevision: Int, metadata: Map[String, String], doAssume: Boolean, handler: AR ⇒ T): Future[(T, Int)] = {
    loadLatest(id, doAssume, basedOnRevision).flatMap { ar ⇒
      val t = handler.apply(ar)
      if (ar.events.isEmpty) {
        Future.successful(t -> ar.revision.get)
      } else {
        recordUpdate(ar, metadata).map(r ⇒ t -> r).recoverWith {
          case _: DuplicateRevisionException ⇒ loadAndUpdate(id, basedOnRevision, metadata, false, handler)
        }
      }
    }
  }
  def update[T](id: AR#ID, basedOnRevision: Int)(updateBlock: AR ⇒ T)(implicit metadata: Map[String, String]): Future[(T, Int)] = try {
    loadAndUpdate(id, basedOnRevision, metadata, true, updateBlock)
  } catch {
    case e: Exception ⇒ Future.failed(e)
  }

}
