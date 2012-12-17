package scuff.es.util

import scuff.ddd._
import scuff._
import java.util.Date
import scuff.es.DuplicateRevisionException
import scuff.es.EventStore

class InMemoryEventStore[ID: Ordering, EVT] extends EventStore[ID, EVT] {
  
  private[this] val pubSub = new PubSub[T]
  
  def subscribe(subscriber: T ⇒ Unit) = pubSub.subscribe(subscriber).asInstanceOf[Subscription]
  
  protected def publish(txn: Transaction) = pubSub.publish(txn)

  private[this] val txnList = collection.mutable.Buffer[T]()

  private def findCurrentRevision(id: ID): Option[Long] = {
    if (txnList.isEmpty) {
      None
    } else {
      Some(txnList.view.filter(_.streamID == id).map(_.revision).max)
    }
  }

  def record(id: ID, revision: Long, events: List[_ <: EVT]) = txnList.synchronized {
    val expectedRevision = findCurrentRevision(id).getOrElse(-1L) + 1L
    if (revision == expectedRevision) {
      val transactionID = BigInt(txnList.size)
      val txn = new Transaction(transactionID, new Timestamp, id, revision, events)
      txnList += txn
      publish(txn)
    } else if (expectedRevision > revision) {
      throw new DuplicateRevisionException
    } else {
      throw new IllegalStateException
    }
  }
  def record(id: ID, events: List[_ <: EVT]): Long = txnList.synchronized {
    val revision = findCurrentRevision(id).getOrElse(-1L) + 1L
    val transactionID = BigInt(txnList.size)
    val txn = new Transaction(transactionID, new Timestamp, id, revision, events)
    txnList += txn
    publish(txn)
    revision
  }

  def replayStream[T](stream: ID)(callback: Iterator[Transaction] ⇒ T): T = txnList.synchronized {
    callback(txnList.iterator.withFilter(t ⇒ t.streamID == stream))
  }
  def replayStreamSince[T](stream: ID, sinceRevision: Long)(callback: Iterator[Transaction] ⇒ T): T = txnList.synchronized {
    callback(txnList.iterator.withFilter(t ⇒ t.streamID == stream && t.revision > sinceRevision))
  }
  def replayStreamTo[T](stream: ID, toRevision: Long)(callback: Iterator[Transaction] ⇒ T): T = txnList.synchronized {
    callback(txnList.iterator.withFilter(t ⇒ t.streamID == stream && t.revision <= toRevision))
  }
  def replayStreamRange[T](stream: ID, revisionRange: collection.immutable.NumericRange[Long])(callback: Iterator[Transaction] ⇒ T): T = txnList.synchronized {
    callback(txnList.iterator.withFilter(t ⇒ t.streamID == stream && revisionRange.contains(t.revision)))
  }
  def replay[T](sinceTransactionID: BigInt)(callback: Iterator[Transaction] ⇒ T): T = txnList.synchronized {
    callback(txnList.drop(sinceTransactionID.toInt + 1).iterator)
  }
  def replaySince[T](fromTime: Date)(callback: Iterator[Transaction] ⇒ T): T = txnList.synchronized {
    callback(txnList.iterator.withFilter(_.timestamp.getTime >= fromTime.getTime))
  }
}