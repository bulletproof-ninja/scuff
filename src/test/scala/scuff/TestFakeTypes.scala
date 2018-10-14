package scuff

import org.junit._, Assert._
object TestFakeTypes {
  sealed abstract class LongWrapper extends FakeType[Long] {
    private[TestFakeTypes] def unwrap(id: Type): Long
  }

  val FooID: LongWrapper = new LongWrapper {
    type Type = Long
    def apply(long: Long): Type = long
    def unwrap(id: Type): Long = id
  }
  type FooID = FooID.Type
  implicit class FooIDOps(private val id: FooID) extends AnyVal {
    def unwrap() = FooID unwrap id
    def toBarID() = BarID(unwrap)
  }

  val BarID: LongWrapper = new LongWrapper {
    type Type = Long
    def apply(long: Long) = long
    def unwrap(wrp: Type) = wrp
  }
  type BarID = BarID.Type
}
class TestFakeTypes {
  import TestFakeTypes._

  @Test
  def foo(): Unit = {

    val foo: FooID = FooID(5L)
    val bar: BarID = foo.toBarID()
    assertEquals(foo, bar)
    assertTrue(foo == bar)
  }
}
