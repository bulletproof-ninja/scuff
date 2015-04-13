package scuff

import org.junit._
import org.junit.Assert._

class TestDoubleDispatch {
  @Test
  def `with return type` {
    trait `123` extends DoubleDispatch[Callback123[Int]]
    class One extends `123` {
      def dispatch(callback: Callback123[Int]) = callback(this)
    }
    class Two extends `123` {
      def dispatch(callback: Callback123[Int]) = callback(this)
    }
    class Three extends `123` {
      def dispatch(callback: Callback123[Int]) = callback(this)
    }
    trait Callback123[A] {
      type RT = A
      def apply(one: One): RT
      def apply(two: Two): RT
      def apply(three: Three): RT
    }
    object Simple extends Callback123[Int] {
      def apply(one: One): Int = 1
      def apply(two: Two): Int = 2
      def apply(three: Three): Int = 3
    }

    assertEquals(1, new One().dispatch(Simple))
    assertEquals(2, new Two().dispatch(Simple))
    assertEquals(3, new Three().dispatch(Simple))
  }
  @Test
  def `no return type` {
    trait `123` extends DoubleDispatch[Callback123]
    class One extends `123` {
      def dispatch(callback: Callback123) = callback(this)
    }
    class Two extends `123` {
      def dispatch(callback: Callback123) = callback(this)
    }
    class Three extends `123` {
      def dispatch(callback: Callback123) = callback(this)
    }
    trait Callback123 {
      type RT = Unit
      def apply(one: One)
      def apply(two: Two)
      def apply(three: Three)
    }
    var last: Int = 0
    object Simple extends Callback123 {
      def apply(one: One) = last = 1
      def apply(two: Two) = last = 2
      def apply(three: Three) = last = 3
    }

    val one = new One()
    val two = new Two()
    val three = new Three()
    one.dispatch(Simple)
    assertEquals(1, last)
    two.dispatch(Simple)
    assertEquals(2, last)
    three.dispatch(Simple)
    assertEquals(3, last)
  }
}
