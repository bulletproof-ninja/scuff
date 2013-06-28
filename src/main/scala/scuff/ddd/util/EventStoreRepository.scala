package scuff.ddd.util

import scuff.ddd._
import scuff.es.EventStore
import scuff.es.DuplicateRevisionException

import scala.concurrent._

/**
 * [[EventStore]]-based [[Repository]] implementation.
 */
abstract class EventStoreRepository[ESID, AR <: AggregateRoot <% CAT, CAT](implicit idConv: AR#ID ⇒ ESID) extends Repository[AR] {

  implicit protected def execCtx: ExecutionContext
  protected val eventStore: EventStore[ESID, _ >: AR#EVT <: DomainEvent, CAT]

  protected type S
  private type TXN = eventStore.Transaction

  /**
   * Create new state mutator instance.
   */
  protected def newStateMutator(snapshotState: Option[S]): DomainStateMutator[AR#EVT, S]

  /**
   * Create an aggregate root instance, using the provided data and revision.
   * @param state The aggregate root state data
   * @param concurrentUpdate These are events that the client is unaware of and therefore it
   * must be determined if there's a conflict with the command given. This allows for fine grained
   * control over concurrent updates if needed, or can either be ignored (for last-man-wins updates) or
   * always prevent update (first-man-wins) for a conservative update strategy.
   */
  protected def newAggregateRoot(id: AR#ID, revision: Long, state: S, concurrentUpdates: List[_ <: AR#EVT]): AR

  protected def loadSnapshot(id: AR#ID): Future[Option[(S, Long)]] = Future.successful(None)
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

  private def loadLatest(id: AR#ID, lastSeenRevision: Long = Long.MaxValue): Future[AR] = {
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
            val replaySinceRev = math.min(snapshotRevision, lastSeenRevision)
            eventStore.replayStreamSince(id, replaySinceRev)(handler)
        }
    }
    futureState.map {
      case None ⇒ throw new UnknownIdException(id)
      case Some((state, revision, concurrentUpdates)) ⇒
        saveSnapshot(id, revision, state)
        newAggregateRoot(id, revision, state, concurrentUpdates)
    }
  }

  def load(id: AR#ID, revision: Option[Long]): Future[AR] = revision match {
    case None ⇒ loadLatest(id)
    case Some(revision) ⇒ loadRevision(id, revision)
  }

  def insert(metadata: Map[String, String])(getAR: ⇒ AR): Future[Unit] = {
    val ar = getAR

    if (ar.revision.nonEmpty) throw new IllegalStateException("Cannot insert. %s already has revision %d".format(ar.id, ar.revision.get))
    if (ar.newEvents.isEmpty) throw new IllegalStateException("Cannot insert. %s has produced no events.".format(ar.id))

    eventStore.record(ar, ar.id, 0L, ar.newEvents, metadata).recover {
      case _: DuplicateRevisionException ⇒ throw new DuplicateIdException(ar.id)
    }
  }

  private[this] def update(ar: AR, metadata: Map[String, String]): Future[Long] = {
    val newRevision = ar.revision.getOrElse(-1L) + 1L
    eventStore.record(ar, ar.id, newRevision, ar.newEvents, metadata).map(_ ⇒ newRevision)
  }

  def update(id: AR#ID, basedOnRevision: Long, metadata: Map[String, String])(handler: AR ⇒ Unit): Future[Long] = {
    loadLatest(id, basedOnRevision).flatMap { ar ⇒
      handler.apply(ar)
      if (ar.newEvents.nonEmpty) {
        update(ar, metadata).recoverWith {
          case _: DuplicateRevisionException ⇒ update(id, basedOnRevision, metadata)(handler)
        }
      } else {
        Future.successful(ar.revision.get)
      }
    }
  }

}
