package scuff

trait ForEach[@specialized(AnyRef, Int, Long, Float, Double) -T]
extends Reduction[T, Unit] {
  def result(): Unit = ()
}
