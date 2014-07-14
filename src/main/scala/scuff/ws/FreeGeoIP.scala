package scuff.ws

import java.net.URL

import scala.util.Try

import scuff.GeoPoint

class FreeGeoIP(urlPrefix: String, parser: FreeGeoIP.Parser) {
  def this(parser: FreeGeoIP.Parser = FreeGeoIP.DefaultJsonParser) = this(s"http://freegeoip.net/${parser.format}/", parser)

  def getGeoPoint(addr: String): Option[GeoPoint] = {
    val url = new URL(urlPrefix concat addr)
    try {
      val reader = toReader(url)
      try {
        parser.parseGeoPoint(reader)
      } finally {
        reader.close()
      }
    } catch {
      case _: java.io.FileNotFoundException ⇒ None
    }
  }

}

object FreeGeoIP {
  trait Parser {
    def format: String
    def parseGeoPoint(content: java.io.BufferedReader): Option[GeoPoint]
  }

  object DefaultJsonParser extends Parser {

    final val ReservedCountryCode = "RD"

    import collection.JavaConverters._
    import java.util.{ Map => jMap, List => jList }

    def format = "json"

    private def toFloat(any: Any) = String.valueOf(any) match {
      case "" | "null" ⇒ None
      case l ⇒ Try(l.toFloat).toOption
    }
    def parseGeoPoint(buf: java.io.BufferedReader): Option[GeoPoint] = {
      val root = JsonParserPool.borrow(_.parse(buf)).asInstanceOf[jMap[String, Any]]
      val latitude = root.get("latitude").asInstanceOf[Number].floatValue
      val longitude = root.get("longitude").asInstanceOf[Number].floatValue
      if (latitude == 0f && longitude == 0f && root.get("country_code") == ReservedCountryCode) {
        None
      } else {
        Some(new GeoPoint(latitude = latitude, longitude = longitude))
      }
    }

  }

}
