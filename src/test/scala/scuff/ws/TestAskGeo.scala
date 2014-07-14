package scuff.ws

import java.io.{BufferedReader, StringReader}
import org.junit._
import Assert._
import scala.util._

class TestAskGeo {
  final val TZResponse = """{"code":0,"message":"ok","data":[{"TimeZone":{"IsInside":"true","AskGeoId":12572,"MinDistanceKm":0.0,"TimeZoneId":"America/New_York","ShortName":"EDT","CurrentOffsetMs":-14400000,"WindowsStandardName":"Eastern Standard Time","InDstNow":"true"},"Point":{"Latitude":40.71,"Longitude":-74.01}}]}"""
  final val ErrResponse = """{"code":1,"message":"bad request"}"""

  @Test
  def timezones() {
    val reader = new BufferedReader(new StringReader(TZResponse))
    AskGeo.DefaultJsonParser.parseTimeZone(reader) match {
      case Seq(tz) => assertEquals("America/New_York", tz.getID)
      case _ => fail("Should return a single timezone")
    }
  }

  @Test
  def badreq() {
    val reader = new BufferedReader(new StringReader(ErrResponse))
    Try(AskGeo.DefaultJsonParser.parseTimeZone(reader)) match {
      case Failure(e) => assertEquals("bad request", e.getMessage)
      case _ => fail("Should throw exception")
    }
  }

}