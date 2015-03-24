package scuff.concurrent

import math.abs
import java.util.concurrent.{ Executor, Executors }
import scala.concurrent._
import scala.concurrent.duration._
import java.util.concurrent.ExecutorService
import java.util.concurrent.TimeUnit

/**
 * `ExecutionContext`, which serializes execution of `Runnable`
 * based on hash. This is useful in some scenarios where
 * parallel processing is generally desirable, but not
 * within a given subset. Defining such subset through a
 * hash will force execution to a particular thread.
 * <p>NOTICE: Use either `execute(Runnable, Int)` or
 * `execute(Runnable)` with `hashCode()` being overridden
 * for the passed `Runnable`. Calling the latter without
 * overriding `hashCode()` will lead to arbitrary thread
 * execution, negating the purpose of this class.
 * @param singleThreadExecutors the `Executor`s used.
 * It is essential, for this class to work, that they are single-threaded.
 * @param shutdownExecutors Executors' shutdown function
 * @param failureReporter The failure reporter function
 */
final class HashPartitionExecutionContext(
  singleThreadExecutors: IndexedSeq[Executor],
  shutdownExecutors: => Future[Unit],
  failureReporter: Throwable => Unit = t => t.printStackTrace(System.err))
    extends ExecutionContextExecutor {

  require(singleThreadExecutors.length > 0, "Must have at least one thread")

  private[this] val threads = singleThreadExecutors.toArray

  /**
   * Runs a block of code on this execution context, using
   * the `hashCode` from the provided `Runnable`.
   */
  def execute(runnable: Runnable): Unit = execute(runnable, runnable.hashCode)

  def execute(hash: Int)(thunk: => Unit): Unit = execute(new Runnable { def run = thunk }, hash)

  /**
   * Runs a block of code on this execution context, using
   * the provided hash.
   */
  def execute(runnable: Runnable, hash: Int): Unit = executorByHash(hash) execute new Runnable {
    def run = try {
      runnable.run()
    } catch {
      case t: Throwable => reportFailure(t)
    }
  }

  @inline
  private def executorByHash(hash: Int) = threads(abs(hash % threads.length))

  def submit[T](hash: Int)(thunk: => T): Future[T] = {
    executorByHash(hash).submit(thunk)
  }

  def reportFailure(t: Throwable) = failureReporter(t)

  private lazy val shutdownFuture = shutdownExecutors
  def shutdown(): Future[Unit] = shutdownFuture
}

object HashPartitionExecutionContext {
  lazy val global = HashPartitionExecutionContext(
    Runtime.getRuntime.availableProcessors,
    Threads.factory(classOf[HashPartitionExecutionContext].getName + ".global"))

  /**
   * @param numThreads Number of threads used for parallelism
   * @param threadFactory The thread factory used to create the threads
   * @param failureReporter Sink for exceptions
   */
  def apply(
    numThreads: Int,
    threadFactory: java.util.concurrent.ThreadFactory = Threads.factory(classOf[HashPartitionExecutionContext].getName),
    failureReporter: Throwable => Unit = t => t.printStackTrace(System.err)) = {
    val threads = new Array[ExecutorService](numThreads)
    for (idx ← 0 until numThreads) {
      threads(idx) = Threads.newSingleThreadExecutor(threadFactory, failureReporter)
    }
      def shutdownAll(exes: Seq[ExecutorService]): Future[Unit] = {
        Future {
          exes.foreach(_.shutdown)
          exes.foreach { exe =>
            exe.awaitTermination(Long.MaxValue, TimeUnit.MILLISECONDS)
          }
        }(Threads.Blocking)
      }
    new HashPartitionExecutionContext(threads, shutdownAll(threads), failureReporter)
  }
}
