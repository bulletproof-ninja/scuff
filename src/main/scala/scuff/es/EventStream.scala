package scuff.es

import java.util.concurrent._
import scuff._
import scuff.ddd._

/**
  * Event stream, which guarantees consistent ordering,
  * even when using distributed protocols that do not.
  */
trait EventStream[ID, EVT, CAT] { self: EventSource[ID, EVT, CAT] ⇒

  protected type TXN = self.Transaction
  private type MS = MonotonicSequencer[Long, TXN]
  private type CS = PersistentEventConsumer[ID, EVT, CAT]

  /**
    * Resume processing transactions from event source.
    * Consumer must be thread safe as event delivery
    * threading is implementation specific.
    */
  def resume(consumer: CS, categories: CAT*): Subscription = {
    val seqConsumer = new TxnSequencer(consumer, categories.toSet)
    // Replay first, to avoid potentially
    // massive out-of-sequence allocation.
    consumer.resumeFrom() match {
      case None ⇒ replay(categories: _*)(_.foreach(seqConsumer))
      case Some(timestamp) ⇒ this.replayFrom(timestamp, categories: _*)(_.foreach(seqConsumer))
    }
    // Then subscribe
    val subscription = subscribe(seqConsumer)
    // Then replay again, to eliminate any possible race condition 
    // between first replay and subscription.
    consumer.resumeFrom() match {
      case None ⇒ // Ignore, nothing has happened, ever
      case Some(timestamp) ⇒ replayFrom(timestamp, categories: _*)(_.foreach(seqConsumer))
    }
    subscription
  }

  private final class TxnSequencer(consumer: CS, filter: Set[CAT]) extends (TXN ⇒ Unit) {
    private[this] val sequencers = new LockFreeConcurrentMap[ID, MS]()
    private def expectedRevision(id: ID) = consumer.lastProcessedRev(id).getOrElse(-1L) + 1L
    private[this] val seqDupeConsumer = (revision: Long, txn: TXN) ⇒ () // Ignore duplicates
    private[this] val seqConsumer = (revision: Long, txn: TXN) ⇒ consumer.consume(txn)

    private def createSequencer(id: ID, nextRevision: Long) = {
      new MonotonicSequencer[Long, TXN](seqConsumer, nextRevision, 0, seqDupeConsumer) {
        override def gapDetected(expectedRevision: Long, actualRevision: Long, txn: TXN) {
          super.gapDetected(expectedRevision, actualRevision, txn)
          replayStreamRange(id, expectedRevision until actualRevision) { txns ⇒
            txns.foreach(ensureSequence)
          }
        }
        override def flushBuffer() = try {
          super.flushBuffer()
        } finally {
          sequencers.remove(id)
        }
      }
    }
    private def ensureSequence(txn: TXN) {
      val sequencer: Option[MS] = sequencers.get(txn.streamId) match {
        case o: Some[_] ⇒ o
        case None ⇒
          val nextRevision = expectedRevision(txn.streamId)
          if (nextRevision == txn.revision) {
            None
          } else {
            val seq = createSequencer(txn.streamId, nextRevision)
            sequencers.putIfAbsent(txn.streamId, seq) match {
              case None ⇒ Some(seq)
              case s: Some[_] ⇒ s
            }
          }
      }
      sequencer match {
        case None ⇒ consumer.consume(txn)
        case Some(sequencer) ⇒ sequencer.apply(txn.revision, txn)
      }
    }
    def apply(txn: TXN) = if (filter.isEmpty || filter.contains(txn.category)) ensureSequence(txn)

    override val toString = consumer.toString
  }
}
