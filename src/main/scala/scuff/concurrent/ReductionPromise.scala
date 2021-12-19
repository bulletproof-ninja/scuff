package scuff.concurrent

import scala.concurrent.{ Future, Promise }
import scala.util.Try
import scala.util.control.NonFatal
import scuff.Reduction

/**
  * Thread-safe reduction.
  * @note Calling `next` is expected to
  * be synchronous. If not, use [[scuff.concurrent.ConcurrentReduction]]
  */
trait ReductionPromise[-T, +R]
extends Reduction[T, Unit] {
  protected[this] val promise = Promise[R]()
  def future: Future[R] = promise.future
}

object ReductionPromise {

  def fold[T, R](init: R, subscribe: Reduction[T, _] => _)(f: (R, T) => R): Future[R] = {
    val callback = new ReductionPromise[T, R] {
      private[this] var acc: R = init
      def next(value: T): Unit = try {
        acc = f(acc, value)
      } catch {
        case NonFatal(th) =>
          promise tryFailure th
      }
      def result(): Unit =
        promise trySuccess acc

    }
    subscribe(callback)
    callback.future
  }
  def foreach[T](subscribe: Reduction[T, _] => _)(each: T => _): Future[Unit] = {
    val promise = ReductionPromise(())(each)
    subscribe(promise)
    promise.future
  }

  def apply[T, R](lazyResult: => R)(each: T => Any): ReductionPromise[T, R] =
    new ReductionPromise[T, R] {
      def next(value: T): Unit =
        try each(value) catch {
          case NonFatal(th) =>
            promise tryFailure th
        }
      def result(): Unit =
        promise tryComplete Try(lazyResult)
    }

  def apply[T, R](delegate: Reduction[T, R]): ReductionPromise[T, R] = delegate match {
    case promise: ReductionPromise[T, R] => promise
    case _ => new ReductionPromise[T, R] {
      def next(v: T) = delegate next v
      def result(): Unit =
        promise complete Try(delegate.result())
    }
  }
}
