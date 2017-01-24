package scuff.web

import scala.collection.JavaConverters._

import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }

case class ETag(tag: String)(weak: Boolean) {
  val headerString = (if (weak) "W/\"" else "\"") concat tag concat "\""
  def addTo(res: HttpServletResponse) = res.addHeader(HttpHeaders.ETag, headerString)
  def setTo(res: HttpServletResponse) = res.setHeader(HttpHeaders.ETag, headerString)
  override def toString = s"${HttpHeaders.ETag}: ${headerString}"
}

object ETag {

  private final val ETagsExtractor = """([wW]\/)?"(\w+)"|(\*)""".r

  def parse(fromHeader: String): List[ETag] = {
    ETagsExtractor.findAllMatchIn(fromHeader).map { m =>
      val weak = m.group(1) eq null
      val value = m.group(3) match {
        case null => m.group(2)
        case all => all
      }
      new ETag(value)(weak)
    }.toList
  }

  private def getETags(header: String, req: HttpServletRequest): List[ETag] = {
    req.getHeaders(header).asScala.toList.flatMap(parse)
  }

  def IfNoneMatch(req: HttpServletRequest): List[ETag] = getETags(HttpHeaders.IfNoneMatch, req)
  def IfMatch(req: HttpServletRequest): List[ETag] = getETags(HttpHeaders.IfMatch, req)

}
