package scuff.concurrent

import scuff._
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.atomic.AtomicReference
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
extends StreamConsumer[T, Future[R]] {
  self: (T => Future[_]) =>

  /**
   * Max. allowed processing time to completion of
   * all `Future`s returned from `this.apply(T)`
   * (which is invoked from `onNext(T)`).
   */
  protected def completionTimeout: FiniteDuration

  /**
   * Produce final result, when done.
   * Called when internal processing is fully completed,
   * i.e. all `Future`s returned from `this.apply(T)` has
   * completed successfully.
   */
  protected def whenDone(): Future[R]

  private[this] val semaphore = new java.util.concurrent.Semaphore(Int.MaxValue)
  protected final def activeCount: Int = Int.MaxValue - semaphore.availablePermits
  private[this] val error = new AtomicReference[Throwable]

  /** Forwarded to `apply(T): Future[_]` */
  def onNext(t: T): Unit = {

    val future: Future[_] = try apply(t) catch {
      case NonFatal(th) => Future failed th
    }

      implicit def ec = Threads.PiggyBack
    if (future.isCompleted) {
      future.failed.foreach(onError)
    } else {
      semaphore.tryAcquire()
      future.onComplete {
        case Failure(th) =>
          onError(th)
          semaphore.release
        case _ => // Success
          semaphore.release
      }
    }

  }

  def onError(th: Throwable): Unit = error.compareAndSet(null, th)

  def onDone(): Future[R] = {

      def whenDone: Future[R] = {
        error.get match {
          case null => this.whenDone()
          case cause => Future failed cause
        }
      }

    if (semaphore tryAcquire Int.MaxValue) whenDone // Fast path, all futures already completed
    else error.get match {
      case th: Throwable => Future failed th // Fail fast on error
      case _ =>
        val instanceName = this.toString()
        val aquired = Threads.onBlockingThread(s"Awaiting completion of $instanceName") {
          val timeout = completionTimeout
          if (!semaphore.tryAcquire(Int.MaxValue, timeout.length, timeout.unit)) {
            val stillActive = this.activeCount
            if (stillActive > 0) throw new TimeoutException(
              s"$instanceName stream consumption still has $stillActive active Futures, $timeout after stream completion. Timeout is either too small or stream possibly incomplete.")
          }
        }
        aquired.flatMap(_ => whenDone)(Threads.PiggyBack)
    }

  }

  override def toString() = s"${this.getClass.getName}@$hashCode"

  protected val mxBean: AsyncStreamConsumer.AsyncStreamConsumerMXBean = new AsyncStreamConsumerBean
  protected class AsyncStreamConsumerBean
  extends AsyncStreamConsumer.AsyncStreamConsumerMXBean {
    def getActiveCount: Int = AsyncStreamConsumer.this.activeCount
  }

  JMX.register(mxBean, toString)

}

object AsyncStreamConsumer {

  private[concurrent] trait AsyncStreamConsumerMXBean {
    def getActiveCount: Int
  }

}
