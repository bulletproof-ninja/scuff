package scuff

import java.util.concurrent._
import scala.concurrent.ExecutionContext

/**
 * `ExecutionContext`, which serializes execution of `Runnable`
 * based on hash. This is useful in some scenarios where
 * parallel processing is generally desirable, but not
 * within a given subset. Defining such subset through a
 * hash will force execution to a particular thread.
 * NOTICE: Use either `execute(Runnable, Int)` or
 * `execute(Runnable)` with `hashCode()` being overridden
 * for the passed `Runnable`. Calling the latter without
 * overriding `hashCode()` will lead to arbitrary thread
 * execution, negating the purpose of this class.
 */
final class HashBasedSerialExecutionContext(
  threads: IndexedSeq[Executor],
  failureReporter: Throwable ⇒ Unit = t ⇒ t.printStackTrace)
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
  final def execute(runnable: Runnable, hash: Int): Unit = threads(hash % numThreads) execute new Runnable {
    def run = try {
      runnable.run()
    } catch {
      case t: Throwable ⇒ reportFailure(t)
    }
  }

  def reportFailure(t: Throwable) = failureReporter(t)
}

object HashBasedSerialExecutionContext {
  val global = HashBasedSerialExecutionContext(Runtime.getRuntime.availableProcessors, Threads.daemonFactory(classOf[HashBasedSerialExecutionContext].getName + ".global"))
  def apply(numThreads: Int, threadFactory: java.util.concurrent.ThreadFactory, failureReporter: Throwable ⇒ Unit = t ⇒ t.printStackTrace) = {
    val threads = new Array[Executor](numThreads)
    for (idx ← 0 until numThreads) {
      threads(idx) = Executors.newSingleThreadExecutor(threadFactory)
    }
    new HashBasedSerialExecutionContext(threads, failureReporter)
  }
}
