//package scuff.eventual.util
//
//import scuff.eventual.EventSource
//import scuff.Threads
//import scala.concurrent.duration.Duration
//import scuff.HashBasedSerialExecutionContext
//import scuff.LockFreeConcurrentMap
//
//object AsyncSequencedTransactionHandler {
//
//  private val SerialExecCtx = HashBasedSerialExecutionContext(numConsumerThreads, EventStream.ConsumerThreadFactory, consumerFailureHandler)
//  private val pendingReplays = new LockFreeConcurrentMap[ID, ScheduledFuture[_]]
//
//  private val ConsumerThreadFactory = Threads.daemonFactory(getClass.getName)
//  import java.util.concurrent._
//  private[this] val scheduler: ScheduledExecutorService = {
//    val exe = new ScheduledThreadPoolExecutor(math.max(1, Runtime.getRuntime.availableProcessors / 2))
//    exe.setKeepAliveTime(2, TimeUnit.MINUTES)
//    exe.allowCoreThreadTimeOut(true)
//    exe
//  }
//  private def schedule(r: Runnable, dur: Duration) = scheduler.schedule(r, dur.toMillis, TimeUnit.MILLISECONDS)
//
//  def apply[ID, EVT, CAT](es: EventSource[ID, EVT, CAT])(consumer: es.Transaction ⇒ Unit): (es.Transaction ⇒ Unit) = {
//    new (es.Transaction ⇒ Unit) with SequencedTransactionHandler[ID, EVT, CAT] with AsyncTransactionHandler[ID, EVT, CAT] {
//      def apply(txn: es.Transaction) {
//
//      }
//      def asyncTransactionCtx = SerialExecCtx
//      def onGapDetected(id: ID, expectedRev: Int, actualRev: Int) {
//        if (!pendingReplays.contains(id)) {
//          val replayer = new Runnable {
//            def run = es.replayStreamRange(id, expectedRev until actualRev)(_.foreach(self))
//          }
//          val futureReplayer = EventStream.schedule(replayer, gapReplayDelay)
//          if (pendingReplays.putIfAbsent(id, futureReplayer).isDefined) futureReplayer.cancel(false)
//        }
//      }
//      def onGapClosed(id: ID) {
//        pendingReplays.get(id) match {
//          case Some(futureReplayer) ⇒
//            futureReplayer.cancel(false)
//            pendingReplays.remove(id, futureReplayer)
//          case _ ⇒ // Ignore
//        }
//      }
//      def nextExpectedRevision(streamId: ID): Int = consumer.nextExpectedRevision(streamId)
//    }
//  }
//}
