package scuff

import java.net.URL

/**
 * Retrieve various geo-location information from geo-points.
 * This class uses the web service at http://www.askgeo.com
 */
class AskGeo(url: String, parser: AskGeo.Parser) {

  def this(apiID: String, apiKey: String, parser: AskGeo.Parser = AskGeo.JsonParser) =
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

      val timeZoneQuery = new URL(url.concat(query.result))
      val conn = timeZoneQuery.openConnection()
      val mimeType = new javax.activation.MimeType(conn.getContentType)
      val charset = Option(mimeType.getParameter("charset")).getOrElse("US-ASCII")
      val is = conn.getInputStream()
      try {
        parser.parse(new java.io.InputStreamReader(is, charset))
      } finally {
        is.close()
      }
    }
  }

}

object AskGeo {
  trait Parser {
    def format: String
    def parse(content: java.io.Reader): Seq[java.util.TimeZone]
  }

  object JsonParser extends Parser {
    val format = "json"

    import scala.util.parsing.json._

    def parse(content: java.io.Reader): Seq[java.util.TimeZone] = {
      val sb = new StringBuilder
      val buf = new java.io.BufferedReader(content)
      var line = buf.readLine()
      while (line != null) {
        sb append line append '\n'
        line = buf.readLine()
      }
      val root = JSON.parseFull(sb.result).get.asInstanceOf[Map[String, Any]]
      val code = root("code")
      if (code != 0) {
        val msg = root.get("message").getOrElse("Error code: " + code).asInstanceOf[String]
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