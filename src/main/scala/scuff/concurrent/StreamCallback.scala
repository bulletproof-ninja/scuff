package scuff.concurrent

trait StreamCallback[T] {
  def onNext(t: T): Unit
  def onError(th: Throwable): Unit
  def onCompleted(): Unit
}
