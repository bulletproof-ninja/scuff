package scuff

trait Reduction[@specialized(AnyRef, Int, Long, Float, Double) -T, +R] {
  def next(t: T): Unit
  def result(): R
}

object Reduction {
  def foreach[T, U](each: T => U): ForEach[T] =
    new ForEach[T] {
      def next(t: T) = each(t)
    }

}
