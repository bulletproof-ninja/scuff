package scuff.concurrent

import scala.concurrent.Future

trait StreamConsumer[@specialized(AnyRef, Int, Long, Float, Double) -T, +R] {
  def onNext(t: T): Unit
  def onError(th: Throwable): Unit
  def onDone(): Future[R]
}

trait AsyncStreamConsumer[-T, +R]
  extends StreamConsumer[T, R] {
  self: (T => Future[Unit]) =>

  import scala.concurrent.Promise
  import scala.concurrent.duration.Duration
  import java.util.concurrent.TimeoutException

  /** Timeout on `onDone()` Future. */
  protected def timeout: Duration

  private[this] val semaphore = new java.util.concurrent.Semaphore(Int.MaxValue)
  private[this] val promise = Promise[Unit]

  def onNext(t: T): Unit = {
    val future = apply(t)
    if (!future.isCompleted) {
      semaphore.tryAcquire()
      future.onComplete(_ => semaphore.release)(Threads.PiggyBack)
    }
  }

  def onError(th: Throwable): Unit = promise failure th

  def whenDone(): Future[R]
  def onDone(): Future[R] = {
    val completed: Future[Unit] =
      if (promise.isCompleted) promise.future
      else Threads.newBlockingThread(s"Awaiting completion of ${getClass.getName}", promise) {
        if (timeout.isFinite) {
          val acquired = semaphore.tryAcquire(Int.MaxValue, timeout.length, timeout.unit)
          if (!acquired) throw new TimeoutException(s"Timed out after $timeout awaiting completion of ${getClass.getName}")
        } else {
          semaphore.acquire(Int.MaxValue)
        }
      }
    completed.flatMap(_ => whenDone())(Threads.PiggyBack)
  }
}
