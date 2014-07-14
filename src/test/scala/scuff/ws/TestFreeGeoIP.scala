package scuff.ws

import java.io.{ BufferedReader, StringReader }
import org.junit._
import Assert._
import scala.util._

class TestFreeGeoIP {
  final val ITResponse = """
{"ip":"46.29.203.52","country_code":"IT","country_name":"Italy","region_code":"","region_name":"","city":"","zipcode":"","latitude":42.8333,"longitude":12.8333,"metro_code":"","area_code":""}
"""
  final val ReservedResponse = """
{"ip":"127.0.0.1","country_code":"RD","country_name":"Reserved","region_code":"","region_name":"","city":"","zipcode":"","latitude":0,"longitude":0,"metro_code":"","area_code":""}
"""

  @Test
  def latlon() {
    val reader = new BufferedReader(new StringReader(ITResponse))
    FreeGeoIP.DefaultJsonParser.parseGeoPoint(reader) match {
      case None => fail("Should return a geopoint")
      case Some(gp) =>
        assertEquals(42.8333f, gp.latitude, 0.000001f)
        assertEquals(12.8333f, gp.longitude, 0.000001f)
    }
  }

  @Test
  def reserved() {
    val reader = new BufferedReader(new StringReader(ReservedResponse))
    val res = FreeGeoIP.DefaultJsonParser.parseGeoPoint(reader)
    assertEquals(None, res)
  }

}
