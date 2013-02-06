package scuff.es.util

import scuff.ddd._
import scuff._
import java.util.Date
import scuff.es.DuplicateRevisionException
import scuff.es.EventStore

class InMemoryEventStore[ID: Ordering,  EVT] extends EventStore[ID, EVT] {
  
  private[this] val pubSub = new PubSub[T]
  
  def subscribe(subscriber: T ⇒ Unit) = pubSub.subscribe(subscriber).asInstanceOf[Subscription]
  
  protected def publish(txn: Transaction) = pubSub.publish(txn)

  private[this] val txnList = collection.mutable.Buffer[T]()

  private def findCurrentRevision(id: ID): Option[Long] = {
    if (txnList.isEmpty) {
      None
    } else {
      Some(txnList.view.filter(_.streamId == id).map(_.revision).max)
    }
  }

  def record(streamId: ID, revision: Long, events: List[_ <: EVT], metadata: Map[String, String]) = txnList.synchronized {
    val expectedRevision = findCurrentRevision(streamId).getOrElse(-1L) + 1L
    if (revision == expectedRevision) {
      val transactionID = BigInt(txnList.size)
      val txn = new Transaction(transactionID, new Timestamp, streamId, revision, metadata, events)
      txnList += txn
      publish(txn)
    } else if (expectedRevision > revision) {
      throw new DuplicateRevisionException
    } else {
      throw new IllegalStateException
    }
  }
  def append(streamId: ID, events: List[_ <: EVT], metadata: Map[String, String]): Long = txnList.synchronized {
    val revision = findCurrentRevision(streamId).getOrElse(-1L) + 1L
    val transactionID = BigInt(txnList.size)
    val txn = new Transaction(transactionID, new Timestamp, streamId, revision, metadata, events)
    txnList += txn
    publish(txn)
    revision
  }

  def replayStream[T](stream: ID)(callback: Iterator[Transaction] ⇒ T): T = txnList.synchronized {
    callback(txnList.iterator.withFilter(t ⇒ t.streamId == stream))
  }
  def replayStreamSince[T](stream: ID, sinceRevision: Long)(callback: Iterator[Transaction] ⇒ T): T = txnList.synchronized {
    callback(txnList.iterator.withFilter(t ⇒ t.streamId == stream && t.revision > sinceRevision))
  }
  def replayStreamTo[T](stream: ID, toRevision: Long)(callback: Iterator[Transaction] ⇒ T): T = txnList.synchronized {
    callback(txnList.iterator.withFilter(t ⇒ t.streamId == stream && t.revision <= toRevision))
  }
  def replayStreamRange[T](stream: ID, revisionRange: collection.immutable.NumericRange[Long])(callback: Iterator[Transaction] ⇒ T): T = txnList.synchronized {
    callback(txnList.iterator.withFilter(t ⇒ t.streamId == stream && revisionRange.contains(t.revision)))
  }
  def replay[T](sinceTransactionID: BigInt)(callback: Iterator[Transaction] ⇒ T): T = txnList.synchronized {
    callback(txnList.drop(sinceTransactionID.toInt + 1).iterator)
  }
  def replaySince[T](fromTime: Date)(callback: Iterator[Transaction] ⇒ T): T = txnList.synchronized {
    callback(txnList.iterator.withFilter(_.timestamp.getTime >= fromTime.getTime))
  }
}