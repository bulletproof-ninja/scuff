package foo.bar

import org.junit._, Assert._
import scuff.Codec

class TestCodec {
  @Test
  def `long/string codec`() {
    val longCodec = Codec.asString(_.toLong)
    assertEquals(42L, longCodec decode "42")
    assertEquals("42", longCodec encode 42)
  }

  @Test
  def `enum/string codec`() {

    object Foo extends Enumeration {
      val Bar, Baz = Value
    }

    val enumCodec = Codec.asString(Foo.withName)
    assertEquals(Foo.Baz, enumCodec decode "Baz")
    assertEquals("Bar", enumCodec encode Foo.Bar)
  }
}
