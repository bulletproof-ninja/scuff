package scuff.concurrent

import java.util.concurrent.{ Executor, ExecutorService, TimeUnit }

import scala.concurrent.{ ExecutionContextExecutor, Future }
import scala.math.abs
import scala.concurrent.ExecutionContext

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
final class PartitionedExecutionContext(
    singleThreadExecutors: Seq[Executor],
    shutdownExecutors: => Future[Unit],
    failureReporter: Throwable => Unit,
    getHash: Runnable => Int = _.hashCode)
  extends ExecutionContextExecutor {

  require(singleThreadExecutors.size > 0, "Must have at least one thread")

  private[this] val threads = singleThreadExecutors.toArray.map {
    case ec: ExecutionContext => ec
    case exe: Executor => ExecutionContext.fromExecutor(exe, failureReporter)
  }

  def singleThread(hash: Int): ExecutionContext with Executor = executorByHash(hash)

  /**
    * Runs a block of code on this execution context, using
    * the `hashCode` from the provided `Runnable`.
    */
  def execute(runnable: Runnable): Unit = execute(runnable, getHash(runnable))

  def execute(hash: Int)(thunk: => Unit): Unit = execute(new Runnable { def run = thunk }, hash)

  /**
    * Runs a block of code on this execution context, using
    * the provided hash.
    */
  def execute(runnable: Runnable, hash: Int): Unit = executorByHash(hash) execute runnable

  @inline
  private def executorByHash(hash: Int) = threads(abs(hash % threads.length))

  def submit[T](hash: Int)(thunk: => T): Future[T] = {
    executorByHash(hash).submit(thunk)
  }

  def reportFailure(t: Throwable) = failureReporter(t)

  private lazy val shutdownFuture = shutdownExecutors
  def shutdown(): Future[Unit] = shutdownFuture
}

final object PartitionedExecutionContext {

  final val Name = classOf[PartitionedExecutionContext].getName

  lazy val global = PartitionedExecutionContext(
    Runtime.getRuntime.availableProcessors,
    (th: Throwable) => th.printStackTrace(System.err),
    Threads.factory(Name + ".global"))

  /**
    * @param numThreads Number of threads used for parallelism
    * @param threadFactory The thread factory used to create the threads
    * @param failureReporter Sink for exceptions
    * @param getHash Function for deriving hash from `Runnable`
    */
  def apply(
      numThreads: Int,
      failureReporter: Throwable => Unit,
      threadFactory: java.util.concurrent.ThreadFactory): PartitionedExecutionContext =
        this(numThreads, failureReporter, threadFactory, _.hashCode)

  /**
    * @param numThreads Number of threads used for parallelism
    * @param failureReporter Sink for exceptions
    * @param getHash Function for deriving hash from `Runnable`
    */
  def apply(
      numThreads: Int,
      failureReporter: Throwable => Unit,
      getHash: Runnable => Int = _.hashCode): PartitionedExecutionContext =
        this(numThreads, failureReporter, Threads.factory(Name), getHash)

  /**
    * @param numThreads Number of threads used for parallelism
    * @param threadFactory The thread factory used to create the threads
    * @param failureReporter Sink for exceptions
    * @param getHash Function for deriving hash from `Runnable`
    */
  def apply(
      numThreads: Int,
      failureReporter: Throwable => Unit,
      threadFactory: java.util.concurrent.ThreadFactory,
      getHash: Runnable => Int): PartitionedExecutionContext = {
    val threads = new Array[ExecutorService](numThreads)
    for (idx <- 0 until numThreads) {
      threads(idx) = Threads.newSingleThreadExecutor(threadFactory, failureReporter)
    }
      def shutdownAll(exes: Seq[ExecutorService]): Future[Unit] =
        Threads.newBlockingThread(
            s"Awaiting $Name shutdown",
            tg = Threads.newThreadGroup(Name, false, failureReporter)) {
          exes.foreach(_.shutdown)
          exes.foreach { exe =>
            exe.awaitTermination(Long.MaxValue, TimeUnit.MILLISECONDS)
          }
        }
    new PartitionedExecutionContext(threads, shutdownAll(threads), failureReporter, getHash)
  }
}
