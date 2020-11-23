package scuff.concurrent

import scuff._
import scala.concurrent._, duration.FiniteDuration
import java.util.concurrent.atomic.{ AtomicReference, AtomicLong }
import java.util.concurrent.TimeoutException
import scala.util.control.NonFatal
import scala.util.Failure

/**
 * Stream consumer extension that delegates
 * `onNext(T): Unit` to `apply(T): Future[_]` and
 * returns the final result on `onDone(): Future[R]`
 * through calling `whenDone()` when all `apply(T)`
 * futures have completed.
 * @tparam T The stream content
 * @tparam R The final stream result
 */
trait AsyncStreamConsumer[-T, +R]
extends (T => Future[_])
with StreamConsumer[T, Future[R]] {

  /**
   * Max. allowed processing time to completion of
   * all `Future`s returned from `this.apply(T)`
   * (which is invoked from `onNext(T)`).
   */
  protected def completionTimeout: FiniteDuration
  protected implicit def executionContext: ExecutionContext

  /**
   * Produce final result, when done.
   * Called when external processing is completed,
   * and either all `Future`s returned from `this.apply(T)` has
   * completed, or the process timed out waiting for all `Future`s
   * to complete. In the latter case, the timeout duration will be
   * provided.
   * @param timedOut Optional timeout, if triggered
   * (i.e. will be `None` if all `Future`s completed)
   * @param errors Any errors from `apply`
   */
  protected def whenDone(timedOut: Option[TimeoutException], errors: List[Throwable]): Future[R]

  private[this] val semaphore = new java.util.concurrent.Semaphore(Int.MaxValue)
  private[this] val counter = new AtomicLong
  protected def activeCount: Int = (Int.MaxValue - semaphore.availablePermits) match {
    case Int.MaxValue => 0 // All finished
    case active => active
  }

  protected def totalCount: Long = counter.get
  protected def errorCount: Int = internalErrors.get.size
  private[this] val externalError = new AtomicReference[Throwable]
  private[this] val internalErrors = new AtomicReference[List[Throwable]](Nil)

  @annotation.tailrec
  private def addInternalError(th: Throwable): Unit = {
    val list = internalErrors.get
    if (!internalErrors.compareAndSet(list, th :: list)) {
      addInternalError(th)
    }
  }

  /** Forwarded to `apply(T): Future[_]` */
  def onNext(t: T): Unit = if (externalError.get == null) {

    counter.incrementAndGet()

    val future: Future[_] = try apply(t) catch {
      case NonFatal(th) => Future failed th
    }

    if (future.isCompleted) {
      future.failed.value.flatMap(_.toOption).foreach(addInternalError)
    } else {
      if (semaphore.tryAcquire()) future.onComplete {
        case Failure(th) =>
          semaphore.release
          addInternalError(th)
        case _ => // Success
          semaphore.release
      } else throw new IllegalStateException(
        s"Cannot process $t after `onDone()` has been called")
    }

  }

  def onError(th: Throwable): Unit =
    externalError.compareAndSet(null, th)

  def onDone(): Future[R] = {

      def whenDone(timeout: Option[TimeoutException]): Future[R] = {
        externalError.get match {
          case null => this.whenDone(timeout, internalErrors.get)
          case cause => Future failed cause
        }
      }

    if (semaphore tryAcquire Int.MaxValue) whenDone(None) // Fast path, all futures already completed
    else externalError.get match {
      case cause: Throwable => Future failed cause // Fail fast on external error
      case _ =>
        val instanceName = this.toString()
        val aquired = Threads.onBlockingThread(s"Awaiting completion of $instanceName") {
          val timeout = completionTimeout
          if (semaphore.tryAcquire(Int.MaxValue, timeout.length, timeout.unit)) None
          else {
            val stillActive = this.activeCount
            if (stillActive > 0) Some {
              new TimeoutException(
                s"$instanceName stream consumption still has $stillActive active Future(s), $timeout after `onDone()` was invoked. Timeout is either too short OR stream is incomplete.")
            } else None
          }
        }

        aquired
          .flatMap(whenDone)
          .andThen { case _ => jmxRegistration.foreach { _.cancel() } }
    }

  }

  override def toString() = s"${this.getClass.getName}@$hashCode"

  /** override with `= new AsyncStreamConsumerBean`. */
  protected def mxBean: AsyncStreamConsumer.AsyncStreamConsumerMXBean = null
  protected class AsyncStreamConsumerBean
  extends AsyncStreamConsumer.AsyncStreamConsumerMXBean {
    def getActiveCount: Int = AsyncStreamConsumer.this.activeCount
    def getErrorCount: Int = AsyncStreamConsumer.this.errorCount
    def getTotalCount: Long = AsyncStreamConsumer.this.totalCount
  }

  private[this] val jmxRegistration: Option[JMX.Registration] =
    Option(mxBean).map(JMX.register(_, AsyncStreamConsumer.this.toString))

}

object AsyncStreamConsumer {

  private[concurrent] trait AsyncStreamConsumerMXBean {
    def getActiveCount: Int
    def getErrorCount: Int
    def getTotalCount: Long
  }

}
trait StrictAsyncStreamConsumer[-T, +R]
extends AsyncStreamConsumer[T, R] {
  protected def whenDone(): Future[R]
  protected def whenDone(timedOut: Option[TimeoutException], errors: List[Throwable]): Future[R] =
    (timedOut orElse errors.headOption) match {
      case Some(failure) => Future failed failure
      case None => whenDone()
    }
}
