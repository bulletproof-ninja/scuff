package scuff.es.util

import scuff.ddd._
import scuff._
import java.util.Date
import scuff.es.DuplicateRevisionException
import scuff.es.EventStore

class InMemoryEventStore[ID, EVT, CAT](evt2cat: EventStore[ID, EVT, CAT]#Transaction ⇒ CAT) extends EventStore[ID, EVT, CAT] {
  
  private[this] val pubSub = new PubSub[CAT, Transaction]()(evt2cat)
  
  def subscribe(subscriber: Transaction ⇒ Unit, filter: CAT ⇒ Boolean) = pubSub.subscribe(subscriber, filter)
  
  protected def publish(txn: Transaction) = pubSub.publish(txn)

  private[this] val txnList = collection.mutable.Buffer[Transaction]()

  private def findCurrentRevision(id: ID): Option[Long] = {
    if (txnList.isEmpty) {
      None
    } else {
      Some(txnList.view.filter(_.streamId == id).map(_.revision).max)
    }
  }

  def record(category: CAT, streamId: ID, revision: Long, events: List[_ <: EVT], metadata: Map[String, String]) = txnList.synchronized {
    val expectedRevision = findCurrentRevision(streamId).getOrElse(-1L) + 1L
    if (revision == expectedRevision) {
      val transactionID = BigInt(txnList.size)
      val txn = new Transaction(transactionID, new Timestamp, category, streamId, revision, metadata, events)
      txnList += txn
      publish(txn)
    } else if (expectedRevision > revision) {
      throw new DuplicateRevisionException
    } else {
      throw new IllegalStateException
    }
  }
  def append(category: CAT, streamId: ID, events: List[_ <: EVT], metadata: Map[String, String]): Long = txnList.synchronized {
    val revision = findCurrentRevision(streamId).getOrElse(-1L) + 1L
    val transactionID = BigInt(txnList.size)
    val txn = new Transaction(transactionID, new Timestamp, category, streamId, revision, metadata, events)
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
  def replay[T](categories: CAT*)(callback: Iterator[Transaction] ⇒ T): T = txnList.synchronized {
    val iter = if (categories.isEmpty) {
      txnList.iterator
    } else {
      val catSet = categories.toSet
      txnList.iterator.withFilter(txn ⇒ catSet.contains(txn.category))
    }
    callback(iter)
  }
  def replayFrom[T](fromTime: Date, categories: CAT*)(callback: Iterator[Transaction] ⇒ T): T = txnList.synchronized {
    val iter = if (categories.isEmpty) {
      txnList.iterator.withFilter(_.timestamp.getTime >= fromTime.getTime)
    } else {
      val catSet = categories.toSet
      txnList.iterator.withFilter(txn ⇒ catSet.contains(txn.category) && txn.timestamp.getTime >= fromTime.getTime)
  }
    callback(iter)
  }
}