package scuff.eventual

import scuff._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util._
import java.util.concurrent.{ TimeUnit, ScheduledFuture }

/**
 * Event stream, which guarantees consistent ordering,
 * even when using distributed protocols that do not.
 * @param es The event source for subscription and replay
 * @param consumerExecCtx The consumer execution context
 * @param gapReplayDelay If revision number gaps are detected, transactions will be replayed after this delay
 * @param consumerFailureReporter Reporting function for consumer failures
 */
final class EventStream[ID, EVT, CAT](
  es: EventSource[ID, EVT, CAT],
  consumerExecCtx: ExecutionContext,
  gapReplayDelay: duration.Duration) {

  type Transaction = EventSource[ID, EVT, CAT]#Transaction

  /**
   * A durable consumer goes through two
   * stages,
   * 1) Historic mode. First time run or
   * resumption after downtime will result
   * in feeding of historic data.
   * 2) Live mode. Consumer should now be
   * finished historically and messages now
   * received are live.
   */
  trait DurableConsumer {
    trait LiveConsumer {
      /**
       * Expected revision for a given stream.
       * If unknown stream, return 0.
       */
      def nextExpectedRevision(stream: ID): Int
      /** Consume live transaction. */
      def consumeLive(txn: Transaction)
    }
    def resumeFrom(): Option[Timestamp]

    /**
     * Called when live consumption starts.
     */
    def onLive(): LiveConsumer

    /** Consume historic transaction. */
    def consumeHistoric(txn: Transaction)

    /** Categories. Empty means all. */
    def categoryFilter: Set[CAT]
  }

  private class ConsumerProxy(consumer: DurableConsumer#LiveConsumer) extends (Transaction ⇒ Unit) {
    private[EventStream] val subPromise = Promise[Subscription]
    private val subscription: Future[Subscription] = subPromise.future

    def apply(txn: Transaction) = try {
      consumer.consumeLive(txn)
    } catch {
      case e: Throwable ⇒
        subscription.foreach(_.cancel)
        throw e
    }
  }

  private[this] val pendingReplays = new LockFreeConcurrentMap[ID, ScheduledFuture[_]]

  private def AsyncSequencedConsumer(consumer: DurableConsumer#LiveConsumer): ConsumerProxy =
    new ConsumerProxy(consumer) with util.SequencedTransactionHandler[ID, EVT, CAT] with util.AsyncTransactionHandler[ID, EVT, CAT] { self: ConsumerProxy ⇒
      def asyncTransactionCtx = consumerExecCtx
      def onGapDetected(id: ID, expectedRev: Int, actualRev: Int) {
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
      def nextExpectedRevision(streamId: ID): Int = consumer.nextExpectedRevision(streamId)
    }

  def resume(consumer: DurableConsumer): Future[Subscription] = {
    import scala.util._

    val starting = new Timestamp
    val categorySet = consumer.categoryFilter
    def categoryFilter(cat: CAT) = categorySet.isEmpty || categorySet.contains(cat)
    def replayConsumer(txns: Iterator[Transaction]): Option[Timestamp] = {
      var lastTs: Long = -1L
      txns.foreach { txn ⇒
        lastTs = txn.timestamp
        consumer.consumeHistoric(txn)
      }
      if (lastTs == -1L) None else Some(new Timestamp(lastTs))
    }
    val futureReplay: Future[Option[Timestamp]] = consumer.resumeFrom match {
      case None ⇒ es.replay(categorySet.toSeq: _*)(replayConsumer)
      case Some(lastTime) ⇒ es.replayFrom(lastTime, categorySet.toSeq: _*)(replayConsumer)
    }
    futureReplay.flatMap { lastTime ⇒
      val safeConsumer = AsyncSequencedConsumer(consumer.onLive())
      val sub = es.subscribe(safeConsumer, categoryFilter)
      safeConsumer.subPromise.success(sub)
      // Close the race condition; replay anything missed between replay and subscription
      es.replayFrom(lastTime.getOrElse(starting), categorySet.toSeq: _*)(_.foreach(safeConsumer)).map(_ ⇒ sub)
    }
  }
}

object EventStream {
  import java.util.concurrent._

  private val ConsumerThreadFactory = Threads.daemonFactory(classOf[EventStream[Any, Any, Any]#DurableConsumer].getName)
  private def schedule(r: Runnable, dur: duration.Duration) = Threads.DefaultScheduler.schedule(r, dur.toMillis, TimeUnit.MILLISECONDS)

  def serializedConsumption[ID, EVT, CAT](
    numThreads: Int,
    es: EventSource[ID, EVT, CAT],
    gapReplayDelay: duration.Duration,
    failureReporter: Throwable ⇒ Unit = (t) ⇒ t.printStackTrace()) = {
    val execCtx = numThreads match {
      case 1 ⇒ ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor(EventStream.ConsumerThreadFactory), failureReporter)
      case n ⇒ HashBasedSerialExecutionContext(n, EventStream.ConsumerThreadFactory, failureReporter)
    }
    new EventStream(es, execCtx, gapReplayDelay)
  }
}
