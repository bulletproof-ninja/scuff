package scuff.web

import org.junit._
import org.junit.Assert._

class TestETags {

  @Test
  def weakEquality(): Unit = {
    val etag = ETag("abc")(weak = true)
    assertEquals("""W/"abc"""", etag.headerString)
    assertEquals(ETag("abc")(weak = false), etag)
    assertEquals("abc", etag.tag)
  }

  @Test
  def parse(): Unit = {
    val abcStrong = ETag.parse(""""abc"""").head
    assertEquals("abc", abcStrong.tag)
    assertFalse(abcStrong.headerString.startsWith("W/"))
    assertFalse(abcStrong.weak)

    val abcStrongLenient = ETag.parse("abc").head
    assertEquals("abc", abcStrongLenient.tag)
    assertFalse(abcStrongLenient.headerString.startsWith("W/"))
    assertFalse(abcStrongLenient.weak)

    val abcWeak = ETag.parse("""W/"abc"""").head
    assertEquals("abc", abcWeak.tag)
    assertTrue(abcWeak.headerString.startsWith("W/"))
    assertTrue(abcWeak.weak)
  }

}
