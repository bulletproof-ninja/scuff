package scuff.ws

import java.net.URL
import scala.util.parsing.json.JSON
import scuff.GeoPoint

/**
 * Retrieve various geo-location information from geo-points.
 * This class uses the web service at http://www.askgeo.com
 */
class AskGeo(urlPrefix: String, parser: AskGeo.Parser) {

  def this(apiID: String, apiKey: String, parser: AskGeo.Parser = AskGeo.DefaultJsonParser) =
    this("http://api.askgeo.com/v1/%s/%s/query.%s".format(apiID, apiKey, parser.format), parser)

  def getTimeZones(points: GeoPoint*): Seq[java.util.TimeZone] = {
    if (points.isEmpty) {
      Seq.empty
    } else {
      val query = new StringBuilder("?databases=TimeZone&points=")
        def appendPoint(p: GeoPoint) {
          query append p.latitude append ',' append p.longitude
        }
      appendPoint(points.head)
      points.tail.foreach { p ⇒
        query append ';'
        appendPoint(p)
      }

      val url = new URL(urlPrefix concat query.result)
      val reader = toReader(url)
      try {
        parser.parseTimeZone(reader)
      } finally {
        reader.close()
      }
    }
  }

}

object AskGeo {
  trait Parser {
    def format: String
    def parseTimeZone(content: java.io.BufferedReader): Seq[java.util.TimeZone]
  }

  object DefaultJsonParser extends Parser {
    def format = "json"

    import scala.util.parsing.json._

    def parseTimeZone(buf: java.io.BufferedReader): Seq[java.util.TimeZone] = {
      val sb = new StringBuilder
      var line = buf.readLine()
      while (line != null) {
        sb append line append '\n'
        line = buf.readLine()
      }
      val root = JSON.parseFull(sb.result).get.asInstanceOf[Map[String, Any]]
      val code = root("code")
      if (code != 0) {
        val msg = root.getOrElse("message", s"Error code: $code").asInstanceOf[String]
        throw new IllegalStateException(msg)
      }
      val data = root("data").asInstanceOf[List[Map[String, Any]]]
      data.map { jsObj ⇒
        val tz = jsObj("TimeZone").asInstanceOf[Map[String, String]]("TimeZoneId")
        java.util.TimeZone.getTimeZone(tz)
      }
    }
  }

}
