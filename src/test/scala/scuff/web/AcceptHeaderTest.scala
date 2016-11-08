package scuff.web

import org.junit._
import org.junit.Assert._
import scuff.MediaType

class AcceptHeaderTest {
  @Test
  def basic {
    val acceptTypes = Set(MediaType("text/html"))
    assertTrue(AcceptHeader("text/html").get.accepts("text/html"))
    assertTrue(AcceptHeader("*/*").get.accepts("text/html"))
    assertTrue(AcceptHeader("text/*").get.accepts("text/html"))
    assertFalse(AcceptHeader("image/*").get.accepts("text/html"))
    assertFalse(AcceptHeader("text/*").get.accepts("image/jpeg"))
    assertTrue(AcceptHeader("*/*").get.accepts("image/jpeg"))
    assertTrue(AcceptHeader("*/*").forall(h => h.acceptsAny(acceptTypes)))
    assertTrue(AcceptHeader("text/*").forall(h => h.acceptsAny(acceptTypes)))
    assertFalse(AcceptHeader("image/*").forall(h => h.acceptsAny(acceptTypes)))
    assertTrue(AcceptHeader("").forall(h => h.acceptsAny(acceptTypes)))
  }

  @Test
  def complex {
    val ah = AcceptHeader("text/html; q=1.0, text/*; q=0.8, image/gif; q=0.6, image/jpeg; q=0.6, image/*; q=0.5").get
    assertTrue(ah.accepts("image/png"))
    assertTrue(ah.accepts("text/plain"))
    assertFalse(ah.accepts("application/json"))
  }

  @Test
  def preference {
    val ah = AcceptHeader("text/html; q=1.0, text/*; q=0.8, image/gif; q=0.6, image/jpeg; q=0.6, image/*; q=0.5, */*; q=0.1").get
    assertTrue(ah.preference.matches("text/html"))
    val ah2 = AcceptHeader("audio/*; q=0.2, audio/basic").get
    assertEquals("audio/basic", ah2.preference.baseType)
  }
  @Test
  def rfc2616_1 {
    val byPreference = AcceptHeader("text/plain; q=0.5, text/html, text/x-dvi; q=0.8, text/x-c").get.preferenceOrdered
    assertEquals(4, byPreference.size)
    assertTrue(byPreference(0).matches("text/html"))
    assertTrue(byPreference(1).matches("text/x-c"))
    assertTrue(byPreference(2).matches("text/x-dvi"))
    assertTrue(byPreference(3).matches("text/plain"))
  }
  @Test
  def rfc2616_2 {
    val byPreference = AcceptHeader("text/*, text/html, text/html;level=1, */*").get.preferenceOrdered
    assertEquals(4, byPreference.size)
    assertEquals("text/html; level=1", byPreference(0).toString)
    assertEquals("text/html", byPreference(1).toString)
    assertEquals("text/*", byPreference(2).toString)
    assertEquals("*/*", byPreference(3).toString)
  }
  @Test
  def rfc2616_3 {
    val ah = AcceptHeader("audio/*; q=0.2, audio/basic").get
    assertEquals("audio/basic", ah.preference.toString)
  }
  @Test
  def rfc2616_4 {
    val ah = AcceptHeader("text/*;q=0.3, text/html;q=0.7, text/html;level=1, text/html;level=2;q=0.4, */*;q=0.5").get
    val byPreference = ah.preferenceOrdered
    assertEquals(5, byPreference.size)
    assertEquals("text/html; level=1", byPreference(0).toString)
    assertTrue(byPreference(1).matches("text/html;level=3"))
    assertTrue(byPreference(1).matches("text/html"))
    assertEquals("*/*; q=0.5", byPreference(2).toString)
    assertTrue(ah.accepts("image/jpeg"))
    assertEquals(MediaType("text/html; level=2; q=0.4").toString, byPreference(3).toString)
  }
  @Test
  def versioned {
    val request = AcceptHeader("application/vnd.scuff+json;v=41, application/vnd.scuff+json;v=42").get
    val expected = MediaType("application/vnd.scuff+json")
    assertTrue(request.accepts(expected))
    assertFalse(request.accepts("application/json"))
    request.withParm(expected, "v", _.toInt).sortBy(_._2).reverse.headOption match {
      case None => fail("Should match")
      case Some((mt, version)) =>
        assertEquals(42, version)
    }
  }
  @Test
  def `vendor match` {
    val plainJson = AcceptHeader("application/json").get
    val responseType = MediaType("application/vnd.scuff+json")
    assertTrue(plainJson.accepts(responseType))
  }
}
