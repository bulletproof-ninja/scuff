package scuff.eventual

import java.util.concurrent.Executors
import java.util.concurrent.ScheduledFuture
import java.util.concurrent.TimeUnit

import scala.annotation.implicitNotFound
import scala.concurrent.Await
import scala.concurrent.Awaitable
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.concurrent.duration.DurationInt

import scuff.HashBasedSerialExecutionContext
import scuff.LockFreeConcurrentMap
import scuff.Subscription
import scuff.Threads
import scuff.Timestamp

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
    consumerCtx: ExecutionContext,
    gapReplayDelay: Duration) {

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

  private val _failedStreams = new LockFreeConcurrentMap[ID, Throwable]

  def failedStreams = _failedStreams.snapshot()

  private class HistoricConsumerProxy(consumer: DurableConsumer) extends (Transaction ⇒ Unit) {
    private[this] var awaitAll: List[Awaitable[_]] = Nil
    private[this] var lastTime = -1L
    def apply(txn: Transaction) = {
      val a = consumerCtx match {
        case ctx: HashBasedSerialExecutionContext =>
          ctx.submit(txn.streamId.hashCode)(consumer consumeHistoric txn)
        case ctx =>
          Future(consumer consumeHistoric txn)(ctx)
      }
      awaitAll ::= a
      lastTime = txn.timestamp
    }

    def lastTimestamp: Option[Timestamp] = {
      val maxAwait = 1.minute
      awaitAll.foreach(Await.ready(_, maxAwait))
      if (lastTime == -1L) None else Some(new Timestamp(lastTime))
    }
  }

  private class LiveConsumerProxy(consumer: DurableConsumer#LiveConsumer) extends (Transaction ⇒ Unit) {
    def apply(txn: Transaction) = consumer.consumeLive(txn)
  }

  private[this] val pendingReplays = new LockFreeConcurrentMap[ID, ScheduledFuture[_]]

  private def proxyHistoricConsumer(consumer: DurableConsumer) = new HistoricConsumerProxy(consumer)

  private def proxyLiveConsumer(consumer: DurableConsumer#LiveConsumer) =
    new LiveConsumerProxy(consumer) with util.FailSafeTransactionHandler[ID, EVT, CAT] with util.SequencedTransactionHandler[ID, EVT, CAT] with util.AsyncTransactionHandler[ID, EVT, CAT] { self: LiveConsumerProxy =>
      def asyncTransactionCtx = consumerCtx
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
      def isFailed(streamId: ID) = _failedStreams.contains(streamId)
      def markFailed(streamId: ID, t: Throwable) {
        _failedStreams.update(streamId, t)
        consumerCtx.reportFailure(t)
      }
    }

  def resume(consumer: DurableConsumer): Future[Subscription] = {
    val starting = new Timestamp
    val categorySet = consumer.categoryFilter
      def categoryFilter(cat: CAT) = categorySet.isEmpty || categorySet.contains(cat)
      def replayConsumer(txns: Iterator[Transaction]): Option[Timestamp] = {
        val histConsumer = proxyHistoricConsumer(consumer)
        txns.foreach(histConsumer)
        histConsumer.lastTimestamp
      }
    val futureReplay: Future[Option[Timestamp]] = consumer.resumeFrom match {
      case None ⇒ es.replay(categorySet.toSeq: _*)(replayConsumer)
      case Some(lastTime) ⇒ es.replayFrom(lastTime, categorySet.toSeq: _*)(replayConsumer)
    }
    val futureSub = futureReplay.flatMap { lastTime ⇒
      val liveConsumer = proxyLiveConsumer(consumer.onLive())
      val sub = es.subscribe(liveConsumer, categoryFilter)
      // Close the race condition; replay anything missed between replay and subscription
      es.replayFrom(lastTime.getOrElse(starting), categorySet.toSeq: _*)(_.foreach(liveConsumer)).map(_ ⇒ sub)(Threads.PiggyBack)
    }(Threads.PiggyBack)
    futureSub.onFailure {
      case t => consumerCtx.reportFailure(t)
    }(Threads.PiggyBack)
    futureSub
  }
}

object EventStream {
  import java.util.concurrent._

  private val ConsumerThreadFactory = Threads.daemonFactory(classOf[EventStream[Any, Any, Any]#DurableConsumer].getName)
  private def schedule(r: Runnable, dur: Duration) = Threads.DefaultScheduler.schedule(r, dur.toMillis, TimeUnit.MILLISECONDS)

  def serializedConsumption[ID, EVT, CAT](
    numThreads: Int,
    es: EventSource[ID, EVT, CAT],
    gapReplayDelay: Duration,
    failureReporter: Throwable ⇒ Unit = (t) ⇒ t.printStackTrace()) = {
    val consumerCtx = numThreads match {
      case 1 ⇒ ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor(EventStream.ConsumerThreadFactory), failureReporter)
      case n ⇒ HashBasedSerialExecutionContext(n, EventStream.ConsumerThreadFactory, failureReporter)
    }
    new EventStream(es, consumerCtx, gapReplayDelay)
  }
}
