package scuff.ws

import java.net.URL

import scuff.geo
import scuff.json._
import scuff.io._
import scala.util.Try

/**
 * Retrieve various geo-location information from geo-points.
 * This class uses the web service at http://www.askgeo.com
 */
class AskGeo(urlPrefix: String, parser: AskGeo.Parser) {

  def this(apiID: String, apiKey: String, parser: AskGeo.Parser = AskGeo.DefaultJsonParser) =
    this("https://api.askgeo.com/v1/%s/%s/query.%s".format(apiID, apiKey, parser.format), parser)

  def getTimeZones(points: geo.Point*): Seq[java.util.TimeZone] = {
    if (points.isEmpty) {
      Seq.empty
    } else {
      val query = new StringBuilder("?databases=TimeZone&points=")
        def appendPoint(p: geo.Point): Unit = {
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

    def format = "json"

    def parseTimeZone(buf: java.io.BufferedReader): Seq[java.util.TimeZone] = {
      val root @ JsObj(_) = JsVal parse buf.copyToCharSeq(1024)
      val code = root.code.asNum.toInt
      if (code != 0) {
        val msg = root.message getOrElse JsStr(s"Error code: $code")
        throw new IllegalStateException(msg.value)
      }
      root.data.asArr.flatMap {
        case obj @ JsObj(_) => Try {
          val tzId = obj.TimeZone.asObj.TimeZoneId.asStr.value
          java.util.TimeZone.getTimeZone(tzId)
        }.toOption
        case _ => None
      }.toSeq
    }
  }

}
