package scuff.ws

import java.net.URL

import scuff.geo

/**
  * Retrieve various geo-location information from geo-points.
  * This class uses the web service at http://www.askgeo.com
  */
class AskGeo(urlPrefix: String, parser: AskGeo.Parser) {

  def this(apiID: String, apiKey: String, parser: AskGeo.Parser = AskGeo.DefaultJsonParser) =
    this("http://api.askgeo.com/v1/%s/%s/query.%s".format(apiID, apiKey, parser.format), parser)

  def getTimeZones(points: geo.Point*): Seq[java.util.TimeZone] = {
    if (points.isEmpty) {
      Seq.empty
    } else {
      val query = new StringBuilder("?databases=TimeZone&points=")
        def appendPoint(p: geo.Point) {
          query append p.latitude append ',' append p.longitude
        }
      appendPoint(points.head)
      points.tail.foreach { p =>
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
    import collection.JavaConverters._
    import java.util.{ Map => jMap, List => jList }

    def format = "json"

    def parseTimeZone(buf: java.io.BufferedReader): Seq[java.util.TimeZone] = {
      val root = JsonParserPool.use(_.parse(buf)).asInstanceOf[jMap[String, Any]]
      val code = root.get("code").asInstanceOf[Number].intValue
      if (code != 0) {
        val msg = Option(root.get("message").asInstanceOf[String]).getOrElse(s"Error code: $code")
        throw new IllegalStateException(msg)
      }
      val data = root.get("data").asInstanceOf[jList[jMap[String, jMap[String, String]]]].asScala
      data.map { jsObj =>
        val tz = jsObj.get("TimeZone").get("TimeZoneId")
        java.util.TimeZone.getTimeZone(tz)
      }
    }
  }

}
