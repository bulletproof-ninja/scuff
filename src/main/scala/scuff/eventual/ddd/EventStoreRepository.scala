package scuff.eventual.ddd

import scuff.ddd._
import scuff.eventual._
import scala.concurrent._
import scuff.SameThreadExecution
import java.util.concurrent.TimeUnit

/**
 * [[EventStore]]-based [[Repository]] implementation.
 */
abstract class EventStoreRepository[ESID, AR <: AggregateRoot <% CAT, CAT](implicit idConv: AR#ID ⇒ ESID, clock: scuff.Clock) extends Repository[AR] {

  implicit protected def execCtx: ExecutionContext = new SameThreadExecution {
    def reportFailure(t: Throwable) = t.printStackTrace()
  }

  protected val eventStore: EventStore[ESID, _ >: AR#EVT <: DomainEvent, CAT]

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
  protected def newAggregateRoot(id: AR#ID, revision: Long, state: S, concurrentUpdates: List[_ <: AR#EVT]): AR

  private[this] val NoFuture = Future.successful(None)
  protected def loadSnapshot(id: AR#ID): Future[Option[(S, Long)]] = NoFuture
  protected def saveSnapshot(id: AR#ID, revision: Long, state: S) {}

  private[this] def loadRevision(id: AR#ID, revision: Long): Future[AR] = {
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
  protected def onLoadNotification(id: AR#ID, revision: Long, timeMs: Long) {}

  /**
   * Optimistically assume any snapshot returned is the most current.
   * This will prevent replaying of any potential events after snapshot.
   * NOTICE: If the assumption fails,
   */
  protected def assumeCurrentSnapshot = false

  private def loadLatest(id: AR#ID, doAssume: Boolean, lastSeenRevision: Long = Long.MaxValue): Future[AR] = {
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
        val applyEventsAfter = snapshotRevision.getOrElse(-1L)
          def handler(txns: Iterator[eventStore.Transaction]): Option[(S, Long, List[_ <: AR#EVT])] = {
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
            if (doAssume && assumeCurrentSnapshot) {
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
        onLoadNotification(id, revision, clock.durationSince(startTime))
        saveSnapshot(id, revision, state)
        newAggregateRoot(id, revision, state, concurrentUpdates)
    }
  }

  def load(id: AR#ID, revision: Option[Long]): Future[AR] = revision match {
    case None ⇒ loadLatest(id, true)
    case Some(revision) ⇒ loadRevision(id, revision)
  }

  def insert(metadata: Map[String, String])(ar: AR): Future[AR] = {
    import scala.util.{ Success, Failure }

    if (ar.revision.nonEmpty) {
      Future.failed(new IllegalStateException("Cannot insert. %s already has revision %d".format(ar.id, ar.revision.get)))
    } else if (ar.events.isEmpty) {
      Future.failed(new IllegalStateException("Cannot insert. %s has produced no events.".format(ar.id)))
    } else {
      val future = eventStore.record(ar, ar.id, 0L, ar.events, metadata)
      future.map(_ ⇒ ar).recover {
        case _: DuplicateRevisionException ⇒ throw new DuplicateIdException(ar.id)
      }
    }
  }

  private[this] def update(ar: AR, metadata: Map[String, String]): Future[Long] = {
    val newRevision = ar.revision.getOrElse(-1L) + 1L
    eventStore.record(ar, ar.id, newRevision, ar.events, metadata).map(_ ⇒ newRevision)
  }

  private def update(id: AR#ID, basedOnRevision: Long, metadata: Map[String, String], doAssume: Boolean, handler: AR ⇒ Unit): Future[Long] = {
    loadLatest(id, doAssume, basedOnRevision).flatMap { ar ⇒
      handler.apply(ar)
      if (ar.events.nonEmpty) {
        update(ar, metadata).recoverWith {
          case _: DuplicateRevisionException ⇒ update(id, basedOnRevision, metadata, false, handler)
        }
      } else {
        Future.successful(ar.revision.get)
      }
    }
  }

  def update(id: AR#ID, basedOnRevision: Long, metadata: Map[String, String])(handler: AR ⇒ Unit): Future[Long] = update(id, basedOnRevision, metadata, true, handler)
}
