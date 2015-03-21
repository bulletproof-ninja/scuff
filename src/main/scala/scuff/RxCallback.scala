package scuff

trait RxCallback[T] {
  def onNext(t: T): Unit
  def onError(t: Throwable): Unit
  def onCompleted(): Unit
}
