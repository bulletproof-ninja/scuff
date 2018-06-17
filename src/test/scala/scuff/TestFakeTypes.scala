package scuff

import org.junit._, Assert._

class TestFakeTypes {
  @Test
  def foo(): Unit = {
    sealed abstract class LongWrapper extends FakeType[Long]

    val FooID: LongWrapper = new LongWrapper {
      type Type = Long
      def apply(long: Long): Type = long
      def unwrap(id: Type): Long = id
    }
    type FooID = FooID.Type

    val BarID: LongWrapper = new LongWrapper {
      type Type = Long
      def apply(long: Long) = long
      def unwrap(wrp: Type) = wrp
    }
    type BarID = BarID.Type

    val foo: FooID = FooID(5L)
    val bar: BarID = BarID(FooID unwrap foo)
    assertEquals(foo, bar)
    assertTrue(foo == bar)
  }
}
