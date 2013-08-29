package scuff.eventual

import scuff._
import scala.concurrent._
import scala.util._
import collection.immutable.NumericRange
import java.util.concurrent.{ TimeUnit, ScheduledFuture }

/**
 * Event stream, which guarantees consistent ordering,
 * even when using distributed protocols that do not.
 * @param es The event source for subscription and replay
 * @param gapReplayDelay If revision number gaps are detected, transactions will be replayed after this delay
 * @param consumerFailureReporter Reporting function for consumer failures
 * @param numConsumerThreads Number of threads to use among available Consumers
 */
final class EventStream[ID, EVT, CAT](
    es: EventSource[ID, EVT, CAT],
    gapReplayDelay: duration.Duration,
    consumerFailureHandler: Throwable ⇒ Unit = (t) ⇒ t.printStackTrace(),
    numConsumerThreads: Int = Runtime.getRuntime.availableProcessors) {

  type Transaction = EventSource[ID, EVT, CAT]#Transaction

  trait Consumer {
    /**
     * Expected revision for a given stream.
     * If unknown stream, return 0.
     */
    def expectedRevision(stream: ID): Long
    /** Consume transaction. */
    def consume(txn: Transaction)

    final def resume(since: Option[Timestamp], categories: CAT*) = EventStream.this.resume(since, this, categories)
  }

  private class ConsumerProxy(consumer: Consumer) extends (Transaction ⇒ Unit) {
    def apply(txn: Transaction) = consumer.consume(txn)
  }

  private[this] val SerialExecCtx = HashBasedSerialExecutionContext(numConsumerThreads, EventStream.ConsumerThreadFactory, consumerFailureHandler)
  private[this] val pendingReplays = new LockFreeConcurrentMap[ID, ScheduledFuture[_]]

  /**
   * Delay before replaying transactions when gaps are detected.
   * This number should be slightly higher than the expected
   * message latency, when using a non-ordered messaging implementation.
   */

  private def AsyncSequencedConsumer(consumer: Consumer) =
    new ConsumerProxy(consumer) with util.SequencedTransactionHandler[ID, EVT, CAT] with util.AsyncTransactionHandler[ID, EVT, CAT] { self: ConsumerProxy ⇒
      def asyncTransactionCtx = SerialExecCtx
      def onGapDetected(id: ID, expectedRev: Long, actualRev: Long) {
        if (!pendingReplays.contains(id)) {
          val replayer = new Runnable {
            def run = es.replayStreamRange(id, expectedRev until actualRev)(_.foreach(self))
          }
          val futureReplayer = EventStream.schedule(replayer, gapReplayDelay)
          if (pendingReplays.putIfAbsent(id, futureReplayer).isDefined) futureReplayer.cancel(false)
        }
      }
      def onGapClosed(id: ID) {
        pendingReplays.get(id) match {
          case Some(futureReplayer) ⇒
            futureReplayer.cancel(false)
            pendingReplays.remove(id, futureReplayer)
          case _ ⇒ // Ignore
        }
      }
      def expectedRevision(streamId: ID): Long = consumer.expectedRevision(streamId)
    }

  private def resume(since: Option[Timestamp], consumer: Consumer, categories: Seq[CAT]): Future[Subscription] = {
    import scala.util._

    val starting = new Timestamp
    val categorySet = categories.toSet
      def categoryFilter(cat: CAT) = categorySet.isEmpty || categorySet.contains(cat)
      def replayConsumer(txns: Iterator[Transaction]): Option[Timestamp] = {
        var last: Timestamp = null
        txns.foreach { txn ⇒
          last = txn.timestamp
          consumer.consume(txn)
        }
        Option(last)
      }
    val futureReplay: Future[Option[Timestamp]] = since match {
      case None ⇒ es.replay(categories: _*)(replayConsumer)
      case Some(lastTime) ⇒ es.replayFrom(lastTime, categories: _*)(replayConsumer)
    }
    futureReplay.flatMap { lastTime ⇒
      val safeConsumer = AsyncSequencedConsumer(consumer)
      val sub = es.subscribe(safeConsumer, categoryFilter)
      // Close the race condition; replay anything missed between replay and subscription
      es.replayFrom(lastTime.getOrElse(starting), categories: _*)(_.foreach(safeConsumer)).map(_ ⇒ sub)(SameThreadExecution)
    }(SameThreadExecution)
  }
}

object EventStream {
  private val ConsumerThreadFactory = Threads.daemonFactory(classOf[EventStream[Any, Any, Any]#Consumer].getName)
  import java.util.concurrent._
  private[this] val scheduler: ScheduledExecutorService = {
    val exe = new ScheduledThreadPoolExecutor(math.max(1, Runtime.getRuntime.availableProcessors / 2))
    exe.setKeepAliveTime(2, TimeUnit.MINUTES)
    exe.allowCoreThreadTimeOut(true)
    exe
  }
  private def schedule(r: Runnable, dur: duration.Duration) = scheduler.schedule(r, dur.toMillis, TimeUnit.MILLISECONDS)
}
