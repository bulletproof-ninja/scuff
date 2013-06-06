package scuff

import javax.servlet.http._

package object web {
  private val RFC822Pool = new ThreadLocal[java.text.SimpleDateFormat] {
    override def initialValue = new java.text.SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss Z", java.util.Locale.US)
  }
  /** ThreadLocal safe date parser. */
  def RFC822 = RFC822Pool.get()

  implicit def enrich(res: HttpServletResponse) = new RichResponse(res)
  implicit def impoverish(res: RichResponse): HttpServletResponse = res.res
  class RichResponse(private[web] val res: HttpServletResponse) {
    def setHeader(etag: ETag): Unit = etag.setTo(res)
    def addHeader(etag: ETag): Unit = etag.addTo(res)
  }
  implicit def enrich(req: HttpServletRequest) = new RichRequest(req)
  implicit def impoverish(req: RichRequest): HttpServletRequest = req.req
  class RichRequest(private[web] val req: HttpServletRequest) {
    def IfNoneMatch() = ETag.IfNoneMatch(req)
    def IfMatch() = ETag.IfNoneMatch(req)
    def Accept() = AcceptHeader(req)
    def IfModifiedSince() = req.getDateHeader(HttpHeaders.IfModifiedSince) match {
      case -1 ⇒ None
      case ims ⇒ Some(ims)
    }
  }

}
