package scuff

import concurrent.ExecutionContext
import math.abs
import java.util.concurrent.{ Executor, Executors }
import java.util.concurrent.ExecutorService
import scala.concurrent._
import scala.concurrent.duration._
import java.util.concurrent.Callable

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
 * @param threads the `Executor`s used.
 * It is essential, for this class to work, that they are single-threaded.
 */
final class HashBasedSerialExecutionContext(
  threads: IndexedSeq[ExecutorService],
  failureReporter: Throwable => Unit = t => t.printStackTrace(System.err))
    extends ExecutionContext {

  private[this] val numThreads = threads.size
  require(numThreads > 0, "Must have at least one thread")

  /**
   * Runs a block of code on this execution context, using
   * the `hashCode` from the provided `Runnable`.
   */
  def execute(runnable: Runnable): Unit = execute(runnable, runnable.hashCode)

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
  private def executorByHash(hash: Int) = threads(abs(hash % numThreads))

  def submit[T](hash: Int)(runnable: => T): Awaitable[T] = new Awaitable[T] {
    val jf = executorByHash(hash) submit new Callable[T] {
      def call = runnable
    }
    def ready(atMost: Duration)(implicit permit: CanAwait) = {
      blocking(jf.get(atMost.toMillis, MILLISECONDS))
      this
    }
    def result(atMost: Duration)(implicit permit: CanAwait): T = blocking(jf.get(atMost.toMillis, MILLISECONDS))
  }

  def reportFailure(t: Throwable) = failureReporter(t)
}

object HashBasedSerialExecutionContext {
  lazy val global = HashBasedSerialExecutionContext(Runtime.getRuntime.availableProcessors, Threads.factory(classOf[HashBasedSerialExecutionContext].getName + ".global"))

  /**
   * @param numThreads Number of threads used for parallelism
   * @param threadFactory The thread factory used to create the threads
   * @param failureReporter Sink for exceptions
   */
  def apply(numThreads: Int, threadFactory: java.util.concurrent.ThreadFactory, failureReporter: Throwable => Unit = t => t.printStackTrace(System.err)) = {
    val threads = new Array[ExecutorService](numThreads)
    for (idx ← 0 until numThreads) {
      threads(idx) = Threads.newSingleThreadExecutor(threadFactory, failureReporter)
    }
    new HashBasedSerialExecutionContext(threads, failureReporter)
  }
}
