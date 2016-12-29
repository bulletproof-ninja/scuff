package scuff.concurrent

import scala.concurrent._
import scala.util.Try

trait StreamPromise[V, +R] extends StreamCallback[V] {

  private[this] val promise = Promise[R]
  final def future = promise.future
  final def onError(t: Throwable): Unit = promise failure t
  final def onCompleted(): Unit = promise complete Try(result)

  protected def result(): R

}

object StreamPromise {

  def fold[V, R](subscribe: StreamCallback[V] => Unit)(init: R)(f: (R, V) => R): Future[R] = {
    val callback = new StreamPromise[V, R] {
      private[this] var acc = init
      def onNext(value: V): Unit = acc = f(acc, value)
      protected def result(): R = acc
    }
    subscribe(callback)
    callback.future
  }
  def foreach[V](subscribe: StreamCallback[V] => Unit)(next: V => Unit): Future[Unit] = {
    val callback = StreamPromise(next)
    subscribe(callback)
    callback.future
  }

  def apply[V](next: V => Unit): StreamPromise[V, Unit] = apply(())(next)
  def apply[V, R](lazyResult: => R)(next: V => Unit) = new StreamPromise[V, R] {
    def onNext(value: V) = next(value)
    protected def result() = lazyResult
  }
}
