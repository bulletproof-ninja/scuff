package scuff

import java.util.concurrent.atomic.AtomicReference

trait Reduction[@specialized(AnyRef, Int, Long, Float, Double) -T, +R] {
  def next(t: T): Unit
  def finished(): R
}

object Reduction {
  def foreach[T, U](each: T => U): ForEach[T] =
    new ForEach[T] {
      def next(t: T) = each(t)
    }

  def fold[T, R](init: R)(thunk: (R, T) => R): Reduction[T, R] = new Reduction[T, R] {
    private[this] val res = new AtomicReference(init)

    @annotation.tailrec
    def next(t: T): Unit = {
      val prev = res.get
      val curr = thunk(prev, t)
      if ( ! res.compareAndSet(prev, curr)) {
        this.next(t)
      }
    }
    def finished(): R =
      res.get
  }

}
