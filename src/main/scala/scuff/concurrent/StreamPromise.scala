package scuff.concurrent

import scala.concurrent.{ Future, Promise }
import scala.util.Try
import scala.util.control.NonFatal
import scuff.StreamConsumer

trait StreamPromise[-V, +R]
extends StreamConsumer[V, Future[R]] {
  def future: Future[R] = promise.future
  def onError(th: Throwable) = promise tryFailure th
  protected[this] val promise = Promise[R]()
}

object StreamPromise {

  def fold[V, R](init: R, subscribe: StreamConsumer[V, Future[R]] => _)(f: (R, V) => R): Future[R] = {
    val callback = new StreamPromise[V, R] {
      private[this] var acc: R = init
      def onNext(value: V): Unit = try {
        acc = f(acc, value)
      } catch {
        case NonFatal(th) =>
          promise tryFailure th
      }
      def onDone(): Future[R] = {
        promise trySuccess acc
        promise.future
      }

    }
    subscribe(callback)
    callback.future
  }
  def foreach[V](subscribe: StreamConsumer[V, _] => _)(next: V => _): Future[Unit] = {
    val promise = StreamPromise(())(next)
    subscribe(promise)
    promise.future
  }

  def apply[V, R](lazyResult: => R)(next: V => Any): StreamPromise[V, R] = new StreamPromise[V, R] {
    def onNext(value: V): Unit = try {
      next(value)
    } catch {
      case NonFatal(th) =>
        promise tryFailure th
    }
    def onDone(): Future[R] = {
      if (!promise.isCompleted) promise tryComplete Try(lazyResult)
      promise.future
    }
  }

  def apply[V, R](delegate: StreamConsumer[V, Future[R]]): StreamPromise[V, R] = delegate match {
    case promise: StreamPromise[V, R] => promise
    case _ => new StreamPromise[V, R] {
      def onNext(v: V) = delegate onNext v
      override def onError(th: Throwable) = {
        super.onError(th)
        delegate onError th
      }
      def onDone(): Future[R] = (promise completeWith delegate.onDone()).future
    }
  }
}
