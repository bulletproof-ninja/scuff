package foo.bar

import org.junit._, Assert._
import scuff.Codec

class TestCodec {
  @Test
  def `long/string codec`(): Unit = {
    val longCodec = Codec.fromString(_.toLong)
    assertEquals(42L, longCodec decode "42")
    assertEquals("42", longCodec encode 42)
  }

  @Test
  def `enum/string codec`(): Unit = {

    object Foo extends Enumeration {
      val Bar, Baz = Value
    }

    val enumCodec = Codec.fromString(Foo.withName)
    assertEquals(Foo.Baz, enumCodec decode "Baz")
    assertEquals("Bar", enumCodec encode Foo.Bar)
  }
}
