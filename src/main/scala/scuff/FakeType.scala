package scuff

/**
  * Fake wrapper type. Provides type-safety without overhead.
  * Canonical usage:
  * {{{
  *   package object foo {
  *
  *     sealed abstract class IntWrapper extends FakeType[Int]
  *
  *     // It's essential that FooInt (and BarInt) are
  *     // explicitly given the abstract IntWrapper type,
  *     // and not the anonymous type that would otherwise
  *     // be inferred.
  *     val FooInt: IntWrapper = new IntWrapper {
  *       type Type = Int
  *       def apply(int: Int): Type = int
  *       def unwrap(foo: Type): Int = foo
  *     }
  *     type FooInt = FooInt.Type
  *
  *     val BarInt: IntWrapper = new IntWrapper {
  *       type Type = Int
  *       def apply(int: Int): Type = int
  *       def unwrap(bar: Type): Int = bar
  *     }
  *     type BarInt = BarInt.Type
  *
  *     val foo: FooInt = 5 // Does not compile
  *     val foo: FooInt = FooInt(5)
  *     val bar: BarInt = foo // Does not compile
  *     val bar = BarInt(5)
  *
  *   }
  *
  * }}}
  * @see https://failex.blogspot.ch/2017/04/the-high-cost-of-anyval-subclasses.html
  */
abstract class FakeType[@specialized T] {
  type Type
  def apply(t: T): Type
}
