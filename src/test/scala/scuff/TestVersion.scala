package scuff

import org.junit._
import org.junit.Assert._

class TestSerialVersionUID {

  type version = SerialVersionUID
  import scuff.{serialVersionUID => version}

  @version(5) case class Foo(hello: String)
  @version(6) case class Bar(world: String)
  case class Baz(beep: Int)

  @Test
  def `verify`(): Unit = {
    for (_ <- 1 to 10) yield {
      assertEquals(5, version[Foo])
    }
  }
  @Test
  def `verify uid`(): Unit = {
    for (_ <- 1 to 10) yield {
      assertEquals(6, version[Bar])
    }
  }

  @Test(expected=classOf[RuntimeException])
  def `failing`(): Unit = {
    version[Baz]
    fail("Should have failed due to lack of annotation")
  }

  @Test
  def `instance`(): Unit = {
    val anyRef: AnyRef = new Foo("Hello")
    assertEquals(5, version(anyRef.getClass))
  }
}
