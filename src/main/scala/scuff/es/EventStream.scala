package scuff.es

//import java.util.concurrent._
import scuff._
import scuff.ddd._
import scala.concurrent._
import scala.util._

/**
 * Event stream, which guarantees consistent ordering,
 * even when using distributed protocols that do not.
 */
trait EventStream[ID, EVT, CAT] { self: EventSource[ID, EVT, CAT] ⇒
  implicit private val ExeCtx = scala.concurrent.ExecutionContext.global

  protected type TXN = self.Transaction
  private type MS = MonotonicSequencer[Long, TXN]
  private type CS = PersistentEventConsumer[ID, EVT, CAT]

  /**
   * Resume processing transactions from event source.
   * Consumer must be thread safe as event delivery
   * threading is implementation specific.
   */
  def resume(extConsumer: CS, categories: CAT*): Future[Subscription] = {
    val seqConsumer: TXN ⇒ Unit = new TransactionSequencer(extConsumer)
      def handler(txns: Iterator[TXN]) = txns.foreach(seqConsumer)
    // Replay first to catch up and avoid
    // large out-of-sequence allocation.
    val futureConsumption = extConsumer.resumeFrom() match {
      case None ⇒ replay(categories: _*)(handler)
      case Some(timestamp) ⇒ this.replayFrom(timestamp, categories: _*)(handler)
    }
    futureConsumption.flatMap { _ ⇒
      // And *then* subscribe
      val categorySet = categories.toSet
      val sub = subscribe(seqConsumer, categorySet.isEmpty || categorySet.contains(_))
      // Then replay again, to eliminate any possible race 
      // condition between first replay and subscription.
      extConsumer.resumeFrom() match {
        case None ⇒ Future.successful(sub)
        case Some(timestamp) ⇒
          replayFrom(timestamp, categories: _*)(handler).map(_ ⇒ sub)
      }
    }
  }

  private[this] val DupeConsumer = (revision: Long, txn: TXN) ⇒ () // Ignore duplicates
  private final class TransactionSequencer(consumer: CS) extends (TXN ⇒ Unit) {
    private[this] val sequencers = new LockFreeConcurrentMap[ID, (MS, SpinLock)]
    private def expectedRevision(id: ID) = consumer.lastProcessedRev(id).getOrElse(-1L) + 1L
    private[this] val seqConsumer = (revision: Long, txn: TXN) ⇒ consumer.consume(txn)

    private def createSequencer(id: ID, nextRevision: Long) = {
      val gapHandler = new GapHandler(id)
      new MonotonicSequencer[Long, TXN](seqConsumer, nextRevision, 0, gapHandler, DupeConsumer)
    }
    private def ensureSequence(txn: TXN) {
      val sequencer: Option[(MS, SpinLock)] = sequencers.get(txn.streamId) match {
        case o: Some[_] ⇒ o
        case None ⇒
          val nextRevision = expectedRevision(txn.streamId)
          if (nextRevision == txn.revision) {
            None
          } else if (nextRevision > txn.revision) {
            return // We ignore dupes
          } else {
            val seq = createSequencer(txn.streamId, nextRevision)
            val lock = new SpinLock
            sequencers.putIfAbsent(txn.streamId, seq -> lock) match {
              case None ⇒ Some(seq -> lock)
              case s: Some[_] ⇒ s
            }
          }
      }
      sequencer match {
        case None ⇒ consumer.consume(txn)
        case Some((sequencer, lock)) ⇒ lock.whenLocked(sequencer.apply(txn.revision, txn))
      }
    }
    def apply(txn: TXN) = ensureSequence(txn)

    override val toString = consumer.toString

    private class GapHandler(id: ID) extends MonotonicSequencer.GapHandler[Long] {
      def gapDetected(expectedRevision: Long, actualRevision: Long): Unit =
        replayStreamRange(id, expectedRevision until actualRevision) { txns ⇒
          txns.foreach(ensureSequence)
        }
      def gapClosed() = sequencers.remove(id)
    }

  }

}
