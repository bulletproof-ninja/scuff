package scuff.web

import javax.servlet.http._

case class ETag(tag: String)(weak: Boolean) {
  val headerValue = (if (weak) "W/\"" else "\"") concat tag concat "\""
  def addTo(res: HttpServletResponse) = res.addHeader(HttpHeaders.ETag, headerValue)
  def setTo(res: HttpServletResponse) = res.setHeader(HttpHeaders.ETag, headerValue)
  override def toString = "%s: %s".format(HttpHeaders.ETag, headerValue)
}

object ETag {

  private final val ETagExtractor = """^([wW]/)?(?:(\w+)|"(\w+)")$""".r.pattern

  def parse(fromHeader: String): ETag = {
    val m = ETagExtractor.matcher(fromHeader)
    require(m.matches(), s"Does not match expected ETag format: $fromHeader")
    val weak = m.group(1) eq null
    val value = m.group(3) match {
      case null => m.group(2)
      case g3 => g3
    }
    new ETag(value)(weak)
  }
  def getETag(header: String, req: HttpServletRequest): Option[ETag] = {
    Option(req.getHeader(header)).map(parse)
  }

  def IfNoneMatch(req: HttpServletRequest): Option[ETag] = getETag(HttpHeaders.IfNoneMatch, req)
  def IfMatch(req: HttpServletRequest): Option[ETag] = getETag(HttpHeaders.IfMatch, req)

}