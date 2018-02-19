package scuff.concurrent

import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

trait StreamConsumer[@specialized(AnyRef, Int, Long, Float, Double) -T, +R] {
  def onNext(t: T): Unit
  def onError(th: Throwable): Unit
  def onDone(): Future[R]
}

trait AsyncStreamConsumer[-T, +R]
  extends StreamConsumer[T, R] {
  self: (T => Future[Unit]) =>

  /** Max. allowed processing time to completion after `onDone()`. */
  protected def completionTimeout: FiniteDuration
  /** Called when processing is fully completed. */
  protected def whenDone(): Future[R]

  private[this] val semaphore = new java.util.concurrent.Semaphore(Int.MaxValue)
  private[this] val error = new AtomicReference[Throwable]

  def onNext(t: T): Unit = {
    val future: Future[Unit] = apply(t)
    if (!future.isCompleted) {
      semaphore.tryAcquire()
      future.onComplete(_ => semaphore.release)(Threads.PiggyBack)
    }
  }

  def onError(th: Throwable): Unit = error.weakCompareAndSet(null, th)

  def onDone(): Future[R] = {
    val completed: Future[Unit] =
      error.get match {
        case null if semaphore.tryAcquire(Int.MaxValue) => // Fast case
          Future successful (())
        case null => // Slow case
          Threads.newBlockingThread(s"Awaiting completion of ${getClass.getName}") {
            val timeout = completionTimeout
            if (!semaphore.tryAcquire(Int.MaxValue, timeout.length, timeout.unit)) {
              throw new TimeoutException(s"Timed out after $timeout awaiting completion of ${getClass.getName}")
            }
          }
        case th => Future failed th
      }
    completed.flatMap(_ => whenDone())(Threads.PiggyBack)
  }
}
