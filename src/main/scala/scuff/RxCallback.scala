package scuff

trait RxCallback[T] {
  def onNext(t: T): Unit
  def onError(t: Throwable): Unit
  def onCompleted(): Unit
}

import scala.concurrent._
import scala.util.Try

object RxFuture {
  def fold[V, R](subscribe: RxCallback[V] => Unit)(init: R)(f: (R, V) => R): Future[R] = {
    val callback = new RxFuture[V, R] {
      private[this] var acc = init
      def onNext(value: V): Unit = acc = f(acc, value)
      protected def whenCompleted(): R = acc
    }
    subscribe(callback)
    callback.future
  }
  def apply[V](thunk: V => Unit) = new RxFuture[V, Unit] {
    def onNext(value: V) = thunk(value)
    protected def whenCompleted() = ()
  }
  def apply[V, R](result: R)(thunk: V => Unit) = new RxFuture[V, R] {
    def onNext(value: V) = thunk(value)
    protected def whenCompleted() = result
  }
}

abstract class RxFuture[V, +R] extends RxCallback[V] {

  private[this] val promise = Promise[R]
  final def future = promise.future
  final def onError(t: Throwable): Unit = promise failure t
  final def onCompleted(): Unit = promise complete Try(whenCompleted)

  def onNext(value: V): Unit
  protected def whenCompleted(): R

}
