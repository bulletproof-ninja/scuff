package scuff.es

import java.util.concurrent._
import scuff._
import scuff.ddd._

/**
  * Event stream, which guarantees consistent ordering,
  * even when using distributed protocols that do not.
  * The callback happens on a single thread, so the consumer
  * does not need to handle synchronization constructs.
  */
trait EventStream[ID, EVT] { self: EventSource[ID, EVT] ⇒

  protected type ES = EventSource[ID, EVT]
  protected type TXN = ES#Transaction
  private type MS = MonotonicSequencer[Long, TXN]
  private type CS = PersistentEventConsumer[ID, EVT]

  /**
    * Resume processing transactions from event source.
    * Consumer must be thread safe as event delivery
    * threading is implementation specific.
    */
  def resume(consumer: CS): Subscription = {
    val seqConsumer = new TxnSequencer(consumer)
    // Replay first, to avoid potentially
    // massive out-of-sequence allocation.
    consumer.lastProcessedTxn() match {
      case None ⇒ replay()(_.foreach(seqConsumer))
      case Some(txnID) ⇒ replay(txnID)(_.foreach(seqConsumer))
    }
    val subscription = subscribe(seqConsumer)
    // Replay again, to eliminate race condition
    consumer.lastProcessedTxn() match {
      case None ⇒ // Ignore, nothing has happened, ever
      case Some(txnID) ⇒ replay(txnID)(_.foreach(seqConsumer))
    }
    subscription
  }

  private final class TxnSequencer(consumer: CS) extends (TXN ⇒ Unit) {
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
      val sequencer: Option[MS] = sequencers.get(txn.streamID) match {
        case o: Some[_] ⇒ o
        case None ⇒
          val nextRevision = expectedRevision(txn.streamID)
          if (nextRevision == txn.revision) {
            None
          } else {
            val seq = createSequencer(txn.streamID, nextRevision)
            sequencers.putIfAbsent(txn.streamID, seq) match {
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
    def apply(txn: TXN) = ensureSequence(txn)

    override val toString = consumer.toString
  }
}
