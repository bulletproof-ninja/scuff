package scuff.ws

import java.net.URL

import scala.util.Try

import scuff.geo

class FreeGeoIP(urlPrefix: String, parser: FreeGeoIP.Parser) {
  def this(parser: FreeGeoIP.Parser = FreeGeoIP.DefaultJsonParser) = this(s"http://freegeoip.net/${parser.format}/", parser)

  def getGeoPoint(addr: String): Option[geo.Point] = {
    val url = new URL(urlPrefix concat addr)
    try {
      val reader = toReader(url)
      try {
        parser.parseGeoPoint(reader)
      } finally {
        reader.close()
      }
    } catch {
      case _: java.io.FileNotFoundException => None
    }
  }

}

object FreeGeoIP {
  trait Parser {
    def format: String
    def parseGeoPoint(content: java.io.BufferedReader): Option[geo.Point]
  }

  object DefaultJsonParser extends Parser {

    final val ReservedCountryCode = "RD"

    import collection.JavaConverters._
    import java.util.{ Map => jMap, List => jList }

    def format = "json"

    def parseGeoPoint(buf: java.io.BufferedReader): Option[geo.Point] = {
      val root = JsonParserPool.use(_.parse(buf)).asInstanceOf[jMap[String, Any]]
      val latitude = root.get("latitude").asInstanceOf[Number].doubleValue
      val longitude = root.get("longitude").asInstanceOf[Number].doubleValue
      if (latitude == 0d && longitude == 0d && root.get("country_code") == ReservedCountryCode) {
        None
      } else {
        Some(new geo.Point(latitude = latitude, longitude = longitude))
      }
    }

  }

}
