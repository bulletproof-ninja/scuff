package scuff.web

import org.junit._
import org.junit.Assert._

class TestETags {
  
  @Test
  def weakEquality {
    val etag = ETag("abc")(weak = true)
    assertEquals("""W/"abc"""", etag.headerValue)
    assertEquals(ETag("abc")(weak = false), etag)
    assertEquals("abc", etag.tag)
  }
  
  @Test
  def parse {
    assertEquals(ETag("abc")(weak = false), ETag.parse(""""abc""""))
    assertEquals(ETag("abc")(weak = false), ETag.parse("""abc"""))
    assertEquals(ETag("abc")(weak = true), ETag.parse("""W/abc"""))
    assertEquals(ETag("abc")(weak = true), ETag.parse("""W/"abc""""))
  }

}