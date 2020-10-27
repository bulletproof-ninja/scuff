package scuff.web

import org.junit._
import org.junit.Assert._

class TestETags {

  @Test
  def weakEquality(): Unit = {
    val etag = ETag("abc")(weak = true)
    assertEquals("""W/"abc"""", etag.headerString)
    assertEquals(ETag("abc")(weak = false), etag)
    assertEquals("abc", etag.value)
  }

  @Test
  def parse(): Unit = {
    val abcStrong = ETag.parse(""""abc"""").head
    assertEquals("abc", abcStrong.value)
    assertFalse(abcStrong.headerString.startsWith("W/"))
    assertFalse(abcStrong.weak)

    assertEquals(Nil, ETag.parse("abc"))

    ETag.parse("""W/"abc", "xyz"""") match {
      case abc :: xyz :: Nil =>
        assertEquals("abc", abc.value)
        assertTrue(abc.headerString.startsWith("W/"))
        assertTrue(abc.weak)
        assertEquals("xyz", xyz.value)
        assertFalse(xyz.headerString.startsWith("W/"))
        assertFalse(xyz.weak)
      case _ => fail("Unexpected")
    }

    ETag.parse("W/\"-1\"").head match {
      case etag @ ETag(value) =>
        assertEquals("-1", value)
        assertTrue(etag.weak)
    }

  }

}
