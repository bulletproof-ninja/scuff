package scuff.web

import javax.servlet.http._

case class ETag(tag: String)(weak: Boolean = false) {
  val value = (if (weak) "W/\"" else "\"") concat tag concat "\""
  def addTo(res: HttpServletResponse) = res.addHeader(HttpHeaders.ETag, value)
  def setTo(res: HttpServletResponse) = res.setHeader(HttpHeaders.ETag, value)
  override def toString = "%s: %s".format(HttpHeaders.ETag, value)
}

object ETag {

  private final val ETagExtractor = """^([wW]/)?(?:(\w+)|"(\w+)")$""".r.pattern

  def apply(tag: String) = new ETag(tag)(false)

  def getETag(name: String, req: HttpServletRequest): Option[ETag] = {
    req.getHeader(HttpHeaders.IfMatch) match {
      case null ⇒ None
      case etagStr ⇒
        val matcher = ETagExtractor.matcher(etagStr)
        matcher.groupCount match {
          case 1 ⇒ Some(new ETag(matcher.group(1))(false))
          case 2 ⇒ Some(new ETag(matcher.group(2))(true))
          case _ ⇒ None
        }
    }
  }

  def IfNoneMatch(req: HttpServletRequest): Option[ETag] = getETag(HttpHeaders.IfNoneMatch, req)
  def IfMatch(req: HttpServletRequest): Option[ETag] = getETag(HttpHeaders.IfMatch, req)

}