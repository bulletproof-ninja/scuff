package scuff.web

import org.junit._
import org.junit.Assert._
import javax.activation.MimeType

class AcceptHeaderTest {
  @Test
  def basic {
    val acceptTypes = Set(new MimeType("text/html"))
    assertTrue(AcceptHeader("text/html").get.matches("text/html"))
    assertTrue(AcceptHeader("*/*").get.matches("text/html"))
    assertTrue(AcceptHeader("text/*").get.matches("text/html"))
    assertFalse(AcceptHeader("image/*").get.matches("text/html"))
    assertFalse(AcceptHeader("text/*").get.matches("image/jpeg"))
    assertTrue(AcceptHeader("*/*").get.matches("image/jpeg"))
    assertTrue(AcceptHeader("*/*").forall(h => h.matchesAny(acceptTypes)))
    assertTrue(AcceptHeader("text/*").forall(h => h.matchesAny(acceptTypes)))
    assertFalse(AcceptHeader("image/*").forall(h => h.matchesAny(acceptTypes)))
    assertTrue(AcceptHeader("").forall(h => h.matchesAny(acceptTypes)))
  }

  @Test
  def complex {
    val ah = AcceptHeader("text/html; q=1.0, text/*; q=0.8, image/gif; q=0.6, image/jpeg; q=0.6, image/*; q=0.5").get
    assertTrue(ah.matches("image/png"))
    assertTrue(ah.matches("text/plain"))
    assertFalse(ah.matches("application/json"))
  }

  @Test
  def preference {
    val ah = AcceptHeader("text/html; q=1.0, text/*; q=0.8, image/gif; q=0.6, image/jpeg; q=0.6, image/*; q=0.5, */*; q=0.1").get
    assertTrue(ah.preference.`match`("text/html"))
  }
  @Test
  def rfc2616_1 {
    val ordered = AcceptHeader("text/plain; q=0.5, text/html, text/x-dvi; q=0.8, text/x-c").get.preferenceOrderd
    assertEquals(4, ordered.size)
    assertTrue(ordered(0).`match`("text/html"))
    assertTrue(ordered(1).`match`("text/x-c"))
    assertTrue(ordered(2).`match`("text/x-dvi"))
    assertTrue(ordered(3).`match`("text/plain"))
  }
  @Test
  def rfc2616_2 {
    val ordered = AcceptHeader("text/*, text/html, text/html;level=1, */*").get.preferenceOrderd
    assertEquals(4, ordered.size)
    assertEquals("text/html; level=1", ordered(0).toString)
    assertEquals("text/html", ordered(1).toString)
    assertEquals("text/*", ordered(2).toString)
    assertEquals("*/*", ordered(3).toString)
  }
  @Test
  def rfc2616_3 {
    val ah = AcceptHeader("audio/*; q=0.2, audio/basic").get
    assertEquals("audio/basic", ah.preference.toString)
  }
  @Test
  def rfc2616_4 {
    val ah = AcceptHeader("text/*;q=0.3, text/html;q=0.7, text/html;level=1, text/html;level=2;q=0.4, */*;q=0.5").get
    val ordered = ah.preferenceOrderd
    assertEquals(5, ordered.size)
    assertEquals("text/html; level=1", ordered(0).toString)
    assertTrue(ordered(1).`match`("text/html;level=3"))
    assertTrue(ordered(1).`match`("text/html"))
    assertEquals("*/*; q=0.5", ordered(2).toString)
    assertTrue(ah.matches("image/jpeg"))
    assertEquals(new MimeType("text/html; level=2; q=0.4").toString, ordered(3).toString)
  }
}
