package scuff

trait StreamConsumer[@specialized(AnyRef, Int, Long, Float, Double) -T, +R] {
  def onNext(t: T): Unit
  def onError(th: Throwable): Unit
  def onDone(): R
}

object StreamConsumer {
  def apply[T, U, UE](err: Throwable => UE)(next: T => U): StreamConsumer[T, Unit] =
    new StreamConsumer[T, Unit] {
      def onNext(t: T) = next(t)
      def onError(th: Throwable) = err(th)
      def onDone(): Unit = ()
    }

}
