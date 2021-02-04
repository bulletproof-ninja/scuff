package scuff.concurrent

import java.util.concurrent._

import scala.concurrent.ExecutionContext

/**
  * Execution context that uses a semaphore to bound
  * capacity and blocks when capacity is reached.
  * This can be useful to conserve memory and as an alternative
  * to limiting queue capacity, which uses exceptions for
  * back-pressure.
  * @param semaphore The semaphore used to control capacity
  * @param underlying Underlying execution context
  */
class BlockingExecutionContext(
  semaphore: Semaphore,
  underlying: ExecutionContext)
extends ExecutionContext
with Executor {

  /**
    * @param queueCapacity The maximum capacity before blocking
    * @param underlying Underlying execution context
    */
  def this(
      queueCapacity: Int,
      underlying: ExecutionContext) =
    this(new Semaphore(queueCapacity), underlying)

  def reportFailure(cause: Throwable): Unit = underlying reportFailure cause
  def execute(command: Runnable): Unit = {
    semaphore.acquire()
    val r = new Runnable {
      def run = {
        semaphore.release()
        command.run()
      }
    }
    try underlying execute r catch {
      case rejection: RejectedExecutionException =>
        semaphore.release()
        throw rejection
    }
  }
}
