package scuff.ws

import java.io.{ BufferedReader, StringReader }
import org.junit._
import Assert._

class TestIPStack {
  final val ITResponse = """
{"latitude":42.8333,"longitude":12.8333}
"""
  final val NullResponse = """
{"latitude":null,"longitude":null}
"""

  @Test
  def latlon(): Unit = {
    val reader = new BufferedReader(new StringReader(ITResponse))
    IPStack.DefaultJsonParser.parseGeoPoint(reader) match {
      case None => fail("Should return a geopoint")
      case Some(gp) =>
        assertEquals(42.8333, gp.latitude, 0.00005)
        assertEquals(12.8333, gp.longitude, 0.00005)
    }
  }

  @Test
  def reserved(): Unit = {
    val reader = new BufferedReader(new StringReader(NullResponse))
    val res = IPStack.DefaultJsonParser.parseGeoPoint(reader)
    assertEquals(None, res)
  }

}
