package scuff.concurrent

import scuff._
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.TimeoutException
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
              val className = {
                getClass().optional(_.getName contains "$anon$")
                  .flatMap[Class[_]](anon => Option(anon.getEnclosingClass))
                  .getOrElse(getClass)
              }.getName
              throw new TimeoutException(
                s"Stream consumption in `$className` is still not finished, $timeout after stream completion, possibly due to either incomplete stream or incomplete state.")
            }
          }
        case th => Future failed th
      }
    completed.flatMap(_ => whenDone())(Threads.PiggyBack)
  }
}
