package scuff

import org.junit._
import org.junit.Assert._

class TestSurgeon {

  @Test
  def foo {
    class Foo(ctor: String) {
      private[this] val foo: String = ctor
    }
    class Bar(ctor: String) extends Foo("foo") {
      private[this] val foo: String = ctor
      private[this] val bar: String = ctor
      private val num = 42
      private val d: java.lang.Double = 1.23d
    }

    val bar = new Bar("baz")
    val surgeon = new Surgeon(bar)
    assertEquals("baz", surgeon.get[String]('bar))
    assertEquals("baz", surgeon.get[String]('foo))
    val expected = Map('bar -> "baz", 'foo -> "baz")
    val actual = surgeon.get(classOf[String])
    assertEquals(expected, actual)
    val num = surgeon.get(classOf[Number]).map(_._2.intValue).sum
    assertEquals(43, num)
    val d = surgeon.get(classOf[Double]).map(_._2.doubleValue).sum
    assertEquals(1.23d, d, 0.0001d)
    val i = surgeon.get(classOf[Integer]).map(_._2.intValue).sum
    assertEquals(42, i)
  }
}