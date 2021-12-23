package scuff.concurrent

import scala.concurrent.{ Future, Promise }
import scala.util.Try
import scala.util.control.NonFatal
import scuff.Reduction

/**
  * Convenience reduction that exposes a `Future`
  * containing the eventual result.
  * @note Calling `next` is expected to
  * be synchronous. If not, use [[scuff.concurrent.ConcurrentReduction]]
  */
trait FutureReduction[-T, +R]
extends Reduction[T, Unit] {
  protected[this] val promise = Promise[R]()
  def future: Future[R] = promise.future
}

object FutureReduction {

  def fold[T, R](init: R, source: Reduction[T, _] => _)(f: (R, T) => R): Future[R] = {
    val callback = new FutureReduction[T, R] {
      @volatile private[this] var acc: R = init
      def next(value: T): Unit = try {
        acc = f(acc, value)
      } catch {
        case NonFatal(th) =>
          promise tryFailure th
      }
      def result(): Unit =
        promise trySuccess acc

    }
    source(callback)
    callback.future
  }
  def foreach[T](source: Reduction[T, _] => _)(each: T => _): Future[Unit] = {
    val reduction = FutureReduction(())(each)
    source(reduction)
    reduction.future
  }

  def apply[T, R](lazyResult: => R)(each: T => Any): FutureReduction[T, R] =
    new FutureReduction[T, R] {
      def next(value: T): Unit =
        try each(value) catch {
          case NonFatal(th) =>
            promise tryFailure th
        }
      def result(): Unit =
        promise tryComplete Try(lazyResult)
    }

  def apply[T, R](delegate: Reduction[T, R]): FutureReduction[T, R] =
    new FutureReduction[T, R] {
      def next(v: T) = delegate next v
      def result(): Unit =
        promise complete Try(delegate.result())
    }

}
