package scuff.concurrent

import java.util.concurrent.{ TimeUnit, ExecutorService, Future => JFuture, Callable, FutureTask, ThreadFactory, ConcurrentLinkedQueue }
import java.util.concurrent.atomic.AtomicReference
import java.util.{ List => JList, Collection }
import scala.concurrent.{ ExecutionContextExecutorService }
import scala.util.control.NonFatal
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.RejectedExecutionException
import java.util.ArrayList
import collection.JavaConverters._
import scala.concurrent.Future
import scala.concurrent.ExecutionContextExecutor
import java.util.concurrent.CountDownLatch

object LockFreeExecutionContext {
  private lazy val DefaultThreadFactory = Threads.factory(classOf[LockFreeExecutionContext].getName)
  def apply(
    numThreads: Int,
    tf: ThreadFactory = DefaultThreadFactory,
    failureReporter: Throwable => Unit = t => t.printStackTrace(System.err),
    whenIdle: => Unit = Thread.`yield`) = {
    val svc = new LockFreeExecutionContext(numThreads, tf, failureReporter, whenIdle)
    svc.start()
    svc
  }

}

/**
 * High throughput executor. Use this for temporary processing
 * of predictably high load, and shut down when done, as it
 * relies on spinning threads, due to the lock-free nature.
 * NOTICE: This class is safe to use for multiple producers,
 * but cannot safely be shut down, unless all producers have
 * stopped. This could in theory be negated by use of external
 * synchronization, but would defeat the purpose of this class.
 * So, either use single producer (and shutdown), or ensure that
 * all production has stopped when shutting down.
 */
class LockFreeExecutionContext private (numThreads: Int, tf: ThreadFactory, failureReporter: Throwable => Unit, whenIdle: => Unit)
    extends ExecutionContextExecutor {

  require(numThreads > 0, s"Must have at least 1 thread. Received $numThreads")

  @volatile private var isShutdown = false

  private[this] val queue = new ConcurrentLinkedQueue[Runnable]

  private[this] val activeThreads = new CountDownLatch(numThreads)

  private[LockFreeExecutionContext] def start() = threads.foreach(_.start)

  private[this] val threads =
    for (_ <- 1 to numThreads) yield tf newThread new Runnable {

      def run = try pollQueue() finally activeThreads.countDown()

      private def pollQueue() {
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

  def execute(runnable: Runnable): Unit = {
    if (isShutdown) throw new RejectedExecutionException("Has been shut down")
    queue offer runnable
  }

  def reportFailure(cause: Throwable): Unit = failureReporter(cause)

  /**
   * Shut down executor, completing jobs already submitted.
   * @return Shutdown completion future
   */
  def shutdown(): Future[Unit] = {
    isShutdown = true
    Threads.Blocking.submit(activeThreads.await)
  }

}
