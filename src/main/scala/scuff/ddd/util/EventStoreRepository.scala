package scuff.ddd.util

import scuff.ddd._
import scuff.es.EventStore
import scuff.es.DuplicateRevisionException

/**
  * [[EventStore]]-based [[Repository]] implementation.
  */
abstract class EventStoreRepository[ESID, AR <: AggregateRoot, CAT](implicit idConv: AR#ID ⇒ ESID, catConv: AR ⇒ CAT) extends Repository[AR] {

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

  protected def loadSnapshot(id: AR#ID): Option[(S, Long)] = None
  protected def saveSnapshot(id: AR#ID, revision: Long, state: S) {}

  private[this] def loadRevision(id: AR#ID, revision: Long): AR = {
    val stateBuilder = newStateMutator(None)
      def handler(txns: Iterator[TXN]) = {
        var last: TXN = null
        txns.foreach { txn ⇒
          txn.events.foreach { e ⇒
            val evt = e.asInstanceOf[AR#EVT]
            stateBuilder(evt)
          }
          last = txn
        }
        Option(last).map(_.revision)
      }
    eventStore.replayStreamTo(id, revision)(handler) match {
      case Some(lastRevision) ⇒ newAggregateRoot(id, lastRevision, stateBuilder.currentState, Nil)
      case None ⇒ throw new UnknownIdException(id)
    }
  }

  private[this] def loadLatest(id: AR#ID, lastSeenRevision: Long = Long.MaxValue): AR = {
    val (stateBuilder, snapshotRevision) = loadSnapshot(id) match {
      case Some((state, revision)) ⇒ (newStateMutator(Some(state)), Some(revision))
      case None ⇒ (newStateMutator(None), None)
    }
    val applyEventsAfter = snapshotRevision.getOrElse(-1L)
      def handler(txns: Iterator[eventStore.Transaction]) = {
        var concurrentUpdates: List[AR#EVT] = Nil
        var last: TXN = null
        txns.foreach { txn ⇒
          txn.events.foreach { e ⇒
            val evt = e.asInstanceOf[AR#EVT]
            if (txn.revision > applyEventsAfter) stateBuilder.apply(evt)
            if (txn.revision > lastSeenRevision) concurrentUpdates :+= evt
          }
          last = txn
        }
        Option(last).map(_.revision -> concurrentUpdates)
      }
    val revision = snapshotRevision match {
      case None ⇒ eventStore.replayStream(id)(handler)
      case Some(snapshotRevision) ⇒
        val replaySinceRev = math.min(snapshotRevision, lastSeenRevision)
        eventStore.replayStreamSince(id, replaySinceRev)(handler)
    }
    revision match {
      case None ⇒ throw new UnknownIdException(id)
      case Some((revision, concurrentUpdates)) ⇒
        val state = stateBuilder.currentState
        saveSnapshot(id, revision, state)
        newAggregateRoot(id, revision, state, concurrentUpdates)
    }
  }

  def load(id: AR#ID, revision: Option[Long]): AR = revision match {
    case None ⇒ loadLatest(id)
    case Some(revision) ⇒ loadRevision(id, revision)
  }

  def insert(ar: AR): Unit = {
    if (ar.committedRevision.isEmpty) try {
      eventStore.record(ar, ar.id, 0L, ar.uncommittedEvents)
    } catch {
      case _: DuplicateRevisionException ⇒ throw new DuplicateIdException(ar.id)
    }
    else throw new IllegalStateException("Cannot insert. %s already has revision %d".format(ar.id, ar.committedRevision))
  }

  private[this] def update(ar: AR, metadata: Map[String, String]): Long = {
    val newRevision = ar.committedRevision.getOrElse(-1L) + 1L
    eventStore.record(ar, ar.id, newRevision, ar.uncommittedEvents, metadata)
    newRevision
  }

  def update(id: AR#ID, basedOnRevision: Long, metadata: Map[String, String])(handler: AR ⇒ Unit): Long = {
    val ar = loadLatest(id, basedOnRevision)
    handler.apply(ar)
    if (!ar.uncommittedEvents.isEmpty) try {
      update(ar, metadata)
    } catch {
      case _: DuplicateRevisionException ⇒ update(id, basedOnRevision, metadata)(handler)
    }
    else {
      ar.committedRevision.get
    }
  }
}
