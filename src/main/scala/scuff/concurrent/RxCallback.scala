package scuff.concurrent

trait RxCallback[T] {
  def onNext(t: T): Unit
  def onError(t: Throwable): Unit
  def onCompleted(): Unit
}

import scala.concurrent._
import scala.util.Try

trait RxFuture[V, +R] extends RxCallback[V] {

  private[this] val promise = Promise[R]
  final def future = promise.future
  final def onError(t: Throwable): Unit = promise failure t
  final def onCompleted(): Unit = promise complete Try(result)

  protected def result(): R

}

object RxFuture {
  def fold[V, R](subscribe: RxCallback[V] => Unit)(init: R)(f: (R, V) => R): Future[R] = {
    val callback = new RxFuture[V, R] {
      private[this] var acc = init
      def onNext(value: V): Unit = acc = f(acc, value)
      protected def result(): R = acc
    }
    subscribe(callback)
    callback.future
  }
  def apply[V](next: V => Unit) = new RxFuture[V, Unit] {
    def onNext(value: V) = next(value)
    protected def result() = ()
  }
  def apply[V, R](result: R)(next: V => Unit) = new RxFuture[V, R] {
    def onNext(value: V) = next(value)
    protected def result() = result
  }
}
