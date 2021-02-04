package scuff.concurrent

import java.util.concurrent.{ Future => _, _ }
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent._
import scala.math.abs

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
  * It is essential, for this class to work as intended,
  * that they are single-threaded.
  * @param shutdownExecutors Executors' shutdown function
  * @param failureReporter The failure reporter function
  */
final class PartitionedExecutionContext(
  singleThreadExecutors: Seq[Executor],
  shutdownExecutors: => Future[Unit],
  failureReporter: Throwable => Unit,
  getHash: Runnable => Int = _.hashCode,
  aggregateQueueCapacity: Int = Int.MaxValue)
extends ExecutionContextExecutor {

  require(singleThreadExecutors.size > 0, "Must have at least one thread")

  private[this] val threads = {
    val threads = singleThreadExecutors.toArray.map {
      case ec: ExecutionContext => ec
      case exe: Executor => ExecutionContext.fromExecutor(exe, failureReporter)
    }
    if (aggregateQueueCapacity == Int.MaxValue || aggregateQueueCapacity <= 0) threads
    else {
      val semaphore = new Semaphore(aggregateQueueCapacity)
      threads.map(new BlockingExecutionContext(semaphore, _))
    }

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

  private[this] val newThreadGroup: (Throwable => Unit) => ThreadGroup = {
    val Counter = new AtomicInteger
    val Name = classOf[PartitionedExecutionContext].getName
    (failureReporter: Throwable => Unit) =>
      Threads.newThreadGroup(s"$Name-${Counter.getAndIncrement}", false, failureReporter)
  }

  def apply(
      numThreads: Int,
      blockingQueueCapacity: Int,
      threadGroup: ThreadGroup)
      : PartitionedExecutionContext =
    this.apply(
      numThreads, blockingQueueCapacity,
      threadGroup,
      Threads factory threadGroup)

  /**
    * @param partitionQueues The individal partition queues.
    * This will equal the number of threads used for parallelism
    * @param failureReporter Sink for exceptions
    * @param threadFactory The thread factory used to create the threads
    */
  def apply(
      partitionQueues: Seq[BlockingQueue[Runnable]],
      failureReporter: Throwable => Unit,
      threadFactory: ThreadFactory): PartitionedExecutionContext = {
    val tg = newThreadGroup(failureReporter)
    this(partitionQueues, tg, threadFactory, _.hashCode)
  }

  /**
    * @param numThreads Number of threads
    * @param blockingQueueCapacity The virtual queue capacity, before blocking on `execute`
    * @param failureReporter Sink for exceptions
    * @param threadFactory The thread factory used to create the threads
    */
  def apply(
      numThreads: Int,
      blockingQueueCapacity: Int,
      failureReporter: Throwable => Unit,
      threadFactory: ThreadFactory): PartitionedExecutionContext = {
    val tg = newThreadGroup(failureReporter)
    this(numThreads, blockingQueueCapacity, tg, threadFactory, _.hashCode)
  }

  /**
    * @param partitionQueues The individal partition queues.
    * This will equal the number of threads used for parallelism
    * @param failureReporter Sink for exceptions
    * @param getHash Function for deriving hash from `Runnable`
    */
  def apply(
      partitionQueues: Seq[BlockingQueue[Runnable]],
      failureReporter: Throwable => Unit,
      getHash: Runnable => Int = _.hashCode): PartitionedExecutionContext = {
    val tg = newThreadGroup(failureReporter)
    this.apply(partitionQueues, tg, Threads.factory(tg), getHash)
  }

  /**
    * @param numThreads Number of threads
    * @param failureReporter Sink for exceptions
    */
  def apply(
      numThreads: Int,
      failureReporter: Throwable => Unit)
      : PartitionedExecutionContext =
    this.apply(numThreads, Int.MaxValue, failureReporter)

  /**
    * @param numThreads Number of threads
    * @param blockingQueueCapacity The virtual queue capacity, before blocking on `execute`
    * @param failureReporter Sink for exceptions
    */
  def apply(
      numThreads: Int,
      blockingQueueCapacity: Int,
      failureReporter: Throwable => Unit)
      : PartitionedExecutionContext = {
    val tg = newThreadGroup(failureReporter)
    val queues = Seq.fill(numThreads)(new LinkedBlockingQueue[Runnable])
    this.apply(queues, blockingQueueCapacity, tg, Threads.factory(tg), _.hashCode)
  }

  /**
    * @param numThreads Number of threads
    * @param blockingQueueCapacity The virtual queue capacity, before blocking on `execute`
    * @param threadGroup Sink for exceptions
    * @param threadFactory The thread factory used to create the threads
    */
  def apply(
      numThreads: Int,
      blockingQueueCapacity: Int,
      threadGroup: ThreadGroup,
      threadFactory: ThreadFactory)
      : PartitionedExecutionContext =
    this.apply(
      Seq.fill(numThreads)(new LinkedBlockingQueue[Runnable]),
      blockingQueueCapacity,
      threadGroup, threadFactory,
      _.hashCode)

  /**
    * @param numThreads Number of threads
    * @param threadGroup Sink for exceptions
    * @param threadFactory The thread factory used to create the threads
    */
  def apply(
      numThreads: Int,
      threadGroup: ThreadGroup,
      threadFactory: ThreadFactory)
      : PartitionedExecutionContext =
    this.apply(
      Seq.fill(numThreads)(new LinkedBlockingQueue[Runnable]),
      Int.MaxValue,
      threadGroup, threadFactory,
      _.hashCode)

  /**
    * @param partitionQueues The individal partition queues.
    * This will equal the number of threads used for parallelism
    * @param threadGroup Sink for exceptions
    * @param threadFactory The thread factory used to create the threads
    */
  def apply(
      partitionQueues: Seq[BlockingQueue[Runnable]],
      threadGroup: ThreadGroup,
      threadFactory: ThreadFactory)
      : PartitionedExecutionContext =
    this.apply(partitionQueues, threadGroup, threadFactory, _.hashCode)

  /**
    * @param numThreads Number of threads
    * @param blockingQueueCapacity The virtual queue capacity, before blocking on `execute`
    * @param threadGroup Sink for exceptions
    * @param threadFactory The thread factory used to create the threads
    * @param getHash Function for deriving hash from `Runnable`
    */
  def apply(
      numThreads: Int,
      blockingQueueCapacity: Int,
      threadGroup: ThreadGroup,
      threadFactory: ThreadFactory,
      getHash: Runnable => Int)
      : PartitionedExecutionContext =
    this.apply(
      Seq.fill(numThreads)(new LinkedBlockingQueue[Runnable]),
      blockingQueueCapacity,
      threadGroup, threadFactory,
      getHash)

  /**
    * @param partitionQueues The individal partition queues.
    * This will equal the number of threads used for parallelism
    * @param threadGroup Sink for exceptions
    * @param threadFactory The thread factory used to create the threads
    * @param getHash Function for deriving hash from `Runnable`
    */
  def apply(
      partitionQueues: Seq[BlockingQueue[Runnable]],
      threadGroup: ThreadGroup,
      threadFactory: ThreadFactory,
      getHash: Runnable => Int)
      : PartitionedExecutionContext =
    this.apply(
      partitionQueues, Int.MaxValue,
      threadGroup, threadFactory,
      getHash)

  /**
    * @param partitionQueues The individal partition queues.
    * This will equal the number of threads used for parallelism
    * @param aggregateQueueCapacity The aggregated (virtual) queue capacity, before blocking on `execute`
    * @param threadGroup Sink for exceptions
    * @param threadFactory The thread factory used to create the threads
    * @param getHash Function for deriving hash from `Runnable`
    */
  def apply(
      partitionQueues: Seq[BlockingQueue[Runnable]],
      aggregateQueueCapacity: Int,
      threadGroup: ThreadGroup,
      getHash: Runnable => Int)
      : PartitionedExecutionContext =
    this.apply(
      partitionQueues, aggregateQueueCapacity,
      threadGroup, Threads.factory(threadGroup),
      getHash)


  /**
    * @param partitionQueues The individal partition queues.
    * This will equal the number of threads used for parallelism
    * @param aggregateQueueCapacity The aggregated (virtual) queue capacity, before blocking on `execute`
    * @param threadGroup Sink for exceptions
    * @param threadFactory The thread factory used to create the threads
    * @param getHash Function for deriving hash from `Runnable`
    */
  def apply(
      partitionQueues: Seq[BlockingQueue[Runnable]],
      aggregateQueueCapacity: Int,
      threadGroup: ThreadGroup,
      threadFactory: ThreadFactory,
      getHash: Runnable => Int)
      : PartitionedExecutionContext = {

    if (aggregateQueueCapacity != Int.MaxValue && aggregateQueueCapacity > 0) {
      require(
        partitionQueues.forall(_.remainingCapacity >= aggregateQueueCapacity)
      , s"Not all queues have capacity to match the aggregate queue capacity of $aggregateQueueCapacity")
    }
    val queues = partitionQueues.toArray
    val numThreads = queues.length
    val threads = new Array[ExecutorService](numThreads)
    val failureReporter = threadGroup.uncaughtException(Thread.currentThread, _)
    for (idx <- 0 until numThreads) {
      threads(idx) = Threads.newSingleThreadExecutor(threadFactory, failureReporter, queues(idx))
    }
        def shutdownAll(exes: Array[ExecutorService]): Future[Unit] =
          Threads.onBlockingThread(
              s"Awaiting ${threadGroup.getName} shutdown",
              tg = threadGroup) {
            exes.foreach(_.shutdown)
            exes.foreach { exe =>
              exe.awaitTermination(Long.MaxValue, TimeUnit.MILLISECONDS)
            }
          }
    new PartitionedExecutionContext(threads.toSeq, shutdownAll(threads), failureReporter, getHash, aggregateQueueCapacity)
  }
}
