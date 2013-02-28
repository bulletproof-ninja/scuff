package scuff

import org.junit._
import org.junit.Assert._

case class Foo(id: Int, name: String)

class TestCache {
  @Test
  def foo {
    val cache = new LRUDirectMemoryCache[String, Foo](10, 1, new JavaSerializer)
    assertEquals(None, cache.lookup("foo"))
    val foo = cache.lookupOrStore("foo")(new Foo(123, "foo"))
    assertEquals(new Foo(123, "foo"), foo)
    assertEquals(Some(new Foo(123, "foo")), cache.lookup("foo"))
    Thread.sleep(1050)
    assertEquals(None, cache.lookup("foo"))
  }
}