package scuff.concurrent

import scala.concurrent.Future

trait StreamConsumer[@specialized(AnyRef, Int, Long, Float, Double) -T, +R] {
  def onNext(t: T): Unit
  def onError(th: Throwable): Unit
  def onDone(): Future[R]
}

trait StreamCallback[-T] extends StreamConsumer[T, Unit] {
  def onDone() = Future(onCompleted)(Threads.PiggyBack)
  def onCompleted(): Unit
}
