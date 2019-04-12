package scuff.concurrent

import java.util.concurrent.{ ConcurrentLinkedQueue, CountDownLatch, RejectedExecutionException, ThreadFactory }

import scala.concurrent.{ ExecutionContextExecutor, Future }
import scala.util.control.NonFatal

object LockFreeExecutionContext {
  /**
    * Queue abstraction.
    */
  trait RunQueue {
    /** Get next or `null`. */
    def poll(): Runnable
    /** Add to queue, if possible. */
    def offer(r: Runnable): Boolean
  }
  private class DefaultQueue extends RunQueue {
    private[this] val queue = new ConcurrentLinkedQueue[Runnable]
    def poll(): Runnable = queue.poll()
    def offer(r: Runnable): Boolean = queue.offer(r)
  }

  private lazy val DefaultThreadFactory = Threads.factory(classOf[LockFreeExecutionContext].getName)
  def apply(
    numThreads: Int,
    tf: ThreadFactory = DefaultThreadFactory,
    failureReporter: Throwable => Unit = t => t.printStackTrace(System.err),
    whenIdle: => Unit = Thread.`yield`,
    queue: RunQueue = new DefaultQueue) = {
    val svc = new LockFreeExecutionContext(numThreads, tf, failureReporter, whenIdle, queue)
    svc.start()
    svc
  }

}

/**
  * High throughput executor. Use this for temporary processing
  * of predictably high load, and shut down when done, as it
  * relies on spinning threads, due to the lock-free nature.
  * NOTICE: This class is safe to use for multiple producers
  * (that is, if the `RunQueue` implementation supports it; the default one does),
  * but cannot safely be shut down, unless all producers have
  * stopped. This could in theory be negated by use of external
  * synchronization, but would defeat the purpose of this class.
  * So, either use single producer (and shutdown when done),
  * or determine, in some way, that all production has stopped before
  * shutting down.
  */
final class LockFreeExecutionContext private (
  consumerThreads: Int,
  tf: ThreadFactory,
  failureReporter: Throwable => Unit,
  whenIdle: => Unit,
  queue: LockFreeExecutionContext.RunQueue)
    extends ExecutionContextExecutor {

  require(consumerThreads > 0, s"Must have at least 1 consumer thread. Received $consumerThreads")

  @volatile private[this] var isShutdown = false

  private[this] val activeThreads = new CountDownLatch(consumerThreads)

  private[LockFreeExecutionContext] def start() = threads.foreach(_.start)

  private[this] val threads =
    for (_ <- 1 to consumerThreads) yield tf newThread new Runnable {

      def run = try pollQueue() finally activeThreads.countDown()

      private def pollQueue(): Unit = {
        while (!Thread.currentThread.isInterrupted) {
          queue.poll() match {
            case null =>
              if (isShutdown) Thread.currentThread.interrupt()
              else whenIdle
            case r =>
              try r.run() catch {
                case NonFatal(e) => reportFailure(e)
              }
          }
        }
      }
    }

  @annotation.tailrec
  def execute(runnable: Runnable): Unit = {
    if (isShutdown) throw new RejectedExecutionException("Has been shut down")
    if (!queue.offer(runnable)) {
      execute(runnable)
    }
  }

  def reportFailure(cause: Throwable): Unit = failureReporter(cause)

  /**
    * Shut down executor, completing jobs already submitted.
    * @return Shutdown completion future
    */
  def shutdown(): Future[Unit] = {
    isShutdown = true
    activeThreads.getCount match {
      case 0 => Future successful (())
      case _ =>
        Threads.onBlockingThread(s"Awaiting ${classOf[LockFreeExecutionContext].getName} shutdown")(activeThreads.await)
    }
  }

}
