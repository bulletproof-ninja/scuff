package scuff.concurrent

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.TimeoutException
import scuff.StreamConsumer
import scala.util.control.NonFatal

trait AsyncStreamConsumer[-T, +R]
  extends StreamConsumer[T, Future[R]] {
  self: (T => Future[_]) =>

  /** Max. allowed processing time to completion after `onDone()`. */
  protected def completionTimeout: FiniteDuration
  /** Called when processing is fully completed. */
  protected def whenDone(): Future[R]

  private[this] val semaphore = new java.util.concurrent.Semaphore(Int.MaxValue)
  private[this] val error = new AtomicReference[Throwable]

  def onNext(t: T): Unit = {
    val future: Future[_] = try apply(t) catch {
      case NonFatal(th) => Future failed th
    }
    if (!future.isCompleted) {
      semaphore.tryAcquire()
      future.onComplete(_ => semaphore.release)(Threads.PiggyBack)
    }
    future.failed.foreach(onError)(Threads.PiggyBack)
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
