package scuff.ws

import java.net.URL

import scuff.io._
import scuff.geo
import scuff.json._

class IPStack(urlPrefix: String, accessKey: String, parser: IPStack.Parser) {
  def this(accessKey: String, parser: IPStack.Parser = IPStack.DefaultJsonParser) =
    this(s"https://api.ipstack.com", accessKey, parser)

  def getGeoPoint(addr: String): Option[geo.Point] = {
    val prefixSep = if (urlPrefix endsWith "/") "" else "/"
    val url = new URL(s"$urlPrefix$prefixSep$addr?access_key=$accessKey&fields=latitude,longitude")
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

object IPStack {
  trait Parser {
    def format: String
    def parseGeoPoint(content: java.io.BufferedReader): Option[geo.Point]
  }

  object DefaultJsonParser extends Parser {

    def format = "json"

    def parseGeoPoint(buf: java.io.BufferedReader): Option[geo.Point] = {
      val root @ JsObj(_) = JsVal parse buf.copyToCharSeq(256)
      root.latitude -> root.longitude match {
        case (JsNum(latitude), JsNum(longitude)) => Some {
          new geo.Point(latitude = latitude.doubleValue, longitude = longitude.doubleValue)
        }
        case _ => None
      }
    }

  }

}
