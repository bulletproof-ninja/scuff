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
   * Called when internal processing is fully completed,
   * i.e. all `Future`s returned from `this.apply(T)` has
   * completed successfully.
   */
  protected def whenDone(): Future[R]

  private[this] val semaphore = new java.util.concurrent.Semaphore(Int.MaxValue)
  private[this] val counter = new AtomicLong
  protected final def activeCount: Int = Int.MaxValue - semaphore.availablePermits
  protected final def totalCount: Long = counter.get
  private[this] val error = new AtomicReference[Throwable]

  /** Forwarded to `apply(T): Future[_]` */
  def onNext(t: T): Unit = if (error.get == null) {

    counter.incrementAndGet()

    val future: Future[_] = try apply(t) catch {
      case NonFatal(th) => Future failed th
    }

    if (future.isCompleted) {
      future.failed.value.flatMap(_.toOption).foreach(onError)
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
      case cause: Throwable => Future failed cause // Fail fast on error
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

        aquired
          .flatMap(_ => whenDone)
          .andThen { case _ => jmxRegistration.foreach { _.cancel() } }
    }

  }

  override def toString() = s"${this.getClass.getName}@$hashCode"

  /** override with `= new AsyncStreamConsumerBean`. */
  protected def mxBean: AsyncStreamConsumer.AsyncStreamConsumerMXBean = null
  protected class AsyncStreamConsumerBean
  extends AsyncStreamConsumer.AsyncStreamConsumerMXBean {
    def getActiveCount: Int = AsyncStreamConsumer.this.activeCount
    def getTotalCount: Long = AsyncStreamConsumer.this.totalCount
  }

  private[this] val jmxRegistration: Option[JMX.Registration] =
    Option(mxBean).map(JMX.register(_, AsyncStreamConsumer.this.toString))

}

object AsyncStreamConsumer {

  private[concurrent] trait AsyncStreamConsumerMXBean {
    def getActiveCount: Int
    def getTotalCount: Long
  }

}
