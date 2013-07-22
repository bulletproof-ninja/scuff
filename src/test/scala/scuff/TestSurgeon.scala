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
    }

    val bar = new Bar("baz")
    val surgeon = new Surgeon(bar)
    assertEquals("baz", surgeon.get[String]('bar))
    assertEquals("baz", surgeon.get[String]('foo))
  }
}