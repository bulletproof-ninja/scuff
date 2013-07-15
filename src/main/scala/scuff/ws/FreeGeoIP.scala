package scuff.ws

import java.net._
import scala.Array
import scala.Option
import scala.util.Try
import scala.util.parsing.json.JSON
import scuff.GeoPoint

class FreeGeoIP(urlPrefix: String, parser: FreeGeoIP.Parser) {
  def this(parser: FreeGeoIP.Parser = FreeGeoIP.DefaultJsonParser) = this("http://freegeoip.net/%s/".format(parser.format), parser)

  def getGeoPoint(addr: String): Option[GeoPoint] = {
    val url = new URL(urlPrefix + addr)
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
    def format = "json"
    import scala.util._
    import parsing.json._
    private def toFloat(any: Any) = String.valueOf(any) match {
      case "" | "null" ⇒ None
      case l ⇒ Try(l.toFloat).toOption
    }
    def parseGeoPoint(buf: java.io.BufferedReader): Option[GeoPoint] = {
      val sb = new StringBuilder
      var line = buf.readLine()
      while (line != null) {
        sb append line append '\n'
        line = buf.readLine()
      }
      val root = JSON.parseFull(sb.result).get.asInstanceOf[Map[String, Any]]
      val latitude = toFloat(root("latitude"))
      val longitude = toFloat(root("longitude"))
      for (latitude ← latitude; longitude ← longitude) yield new GeoPoint(latitude = latitude, longitude = longitude)
    }

  }

}
