package scuff.concurrent

import scuff._

import java.util.concurrent._
import scala.concurrent.ExecutionContextExecutorService

private[concurrent] final class SingleThreadExecutor(
  threadFactory: ThreadFactory,
  queue: BlockingQueue[Runnable],
  failureReporter: Option[Throwable => Unit],
  rejectionHandler: RejectedExecutionHandler)
extends ThreadPoolExecutor(1, 1, 0L, TimeUnit.MILLISECONDS, queue, threadFactory, rejectionHandler)
with ExecutionContextExecutorService
with FailureReporting {

  private[this] val reportException = failureReporter || super.reportFailure _
  override def reportFailure(th: Throwable) = reportException(th)

}
