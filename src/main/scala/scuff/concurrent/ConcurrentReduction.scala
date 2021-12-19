package scuff.concurrent

import scuff._
import scala.concurrent._, duration.FiniteDuration
import java.util.concurrent.atomic.{ AtomicReference, AtomicLong }
import java.util.concurrent.TimeoutException
import scala.util.control.NonFatal
import scala.util.Failure

/**
 * Reduction that allows each individual item `T` to run
 * concurrently.
 * @tparam T The stream content
 * @tparam R The final stream result
 */
trait ConcurrentReduction[-T, R]
extends Reduction[T, Future[R]] {

  /**
   * Max. allowed processing time to completion of
   * all `Future`s returned from `this.apply(T)`
   * (which is invoked from `next(T)`).
   */
  protected def completionTimeout: FiniteDuration
  protected implicit def executionContext: ExecutionContext

  /**
   * Produce final result.
   * Called when external processing is completed,
   * and either all `Future`s returned from `this.asyncNext(T)` has
   * completed, or the process timed out waiting for all `Future`s
   * to complete. In the latter case, the timeout duration will be
   * provided.
   * @param timedOut Optional timeout, if triggered
   * (i.e. will be `None` if all `Future`s completed)
   * @param errors Any errors from `asyncNext`
   */
  protected def asyncResult(timedOut: Option[TimeoutException], errors: List[Throwable]): Future[R]

  private[this] val semaphore = new java.util.concurrent.Semaphore(Int.MaxValue)
  private[this] val counter = new AtomicLong
  protected def activeCount: Int = (Int.MaxValue - semaphore.availablePermits) match {
    case Int.MaxValue => 0 // All finished
    case active => active
  }

  protected def totalCount: Long = counter.get
  protected def errorCount: Int = internalErrors.get.size
  protected final val promise = Promise[R]()
  private[this] val internalErrors = new AtomicReference[List[Throwable]](Nil)

  @annotation.tailrec
  private def addInternalError(th: Throwable): Unit = {
    val list = internalErrors.get
    if (!internalErrors.compareAndSet(list, th :: list)) {
      addInternalError(th)
    }
  }

  def asyncNext(t: T): Future[Any]

  /** Forwarded to `asyncNext(T): Future[Any]` */
  def next(t: T): Unit =
    if (promise.isCompleted) {
      throw new IllegalStateException(s"Consumption completed: ${promise.future.value.get}")
    } else {

      counter.incrementAndGet()

      val future: Future[_] = try asyncNext(t) catch {
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
          s"Cannot process after stream has completed: $t")
      }

    }

  def result(): Future[R] = {

    if (!promise.isCompleted) promise.completeWith {
      if (semaphore tryAcquire Int.MaxValue) {
        asyncResult(None, internalErrors.get)
      } else {
        val instanceName = this.toString()
        val aquired = Threads.onBlockingThread(s"Awaiting completion of $instanceName") {
          val timeout = completionTimeout
          if (semaphore.tryAcquire(Int.MaxValue, timeout.length, timeout.unit)) None
          else {
            val stillActive = this.activeCount
            if (stillActive > 0) Some {
              new TimeoutException(
                s"$instanceName stream consumption still has $stillActive active Future(s), $timeout after `result()` was invoked. Timeout is either too short OR stream is incomplete.")
            } else None
          }
        }

        aquired
          .flatMap(asyncResult(_, internalErrors.get))
          .andThen { case _ => jmxRegistration.foreach { _.cancel() } }

      }
    }

    promise.future
  }

  override def toString() = s"${this.getClass.getName}@$hashCode"

  /** override with `= new AsyncStreamConsumerBean`. */
  protected def mxBean: ConcurrentReduction.ConcurrentReducerMXBean = null
  protected class AsyncStreamConsumerBean
  extends ConcurrentReduction.ConcurrentReducerMXBean {
    def getActiveCount: Int = ConcurrentReduction.this.activeCount
    def getErrorCount: Int = ConcurrentReduction.this.errorCount
    def getTotalCount: Long = ConcurrentReduction.this.totalCount
  }

  private[this] val jmxRegistration: Option[JMX.Registration] =
    Option(mxBean).map(JMX.register(_, ConcurrentReduction.this.toString))

}

object ConcurrentReduction {

  private[concurrent] trait ConcurrentReducerMXBean {
    def getActiveCount: Int
    def getErrorCount: Int
    def getTotalCount: Long
  }

}

/**
  * [[scuff.concurrent.ConcurrentReduction]] that does not accept
  * timeout or failures.
  */
trait NoFailuresAccepted[R] {
  reducer: ConcurrentReduction[_, R] =>

  protected def asyncResult(): Future[R]
  protected def asyncResult(timedOut: Option[TimeoutException], errors: List[Throwable]): Future[R] =
    (timedOut orElse errors.headOption) match {
      case Some(failure) => Future failed failure
      case None => asyncResult()
    }

}
