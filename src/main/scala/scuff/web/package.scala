package scuff

import javax.servlet.http._
import java.util.Locale
import java.net.InetAddress
import javax.servlet.ServletRequest
import language.implicitConversions
import javax.servlet.ServletResponse

package object web {
  private val RFC822Pool = new ThreadLocal[java.text.SimpleDateFormat] {
    override def initialValue = new java.text.SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss Z", java.util.Locale.US)
  }
  /** ThreadLocal safe date parser. */
  def RFC822 = RFC822Pool.get()

  implicit def toHttpReq(req: ServletRequest) = req.asInstanceOf[HttpServletRequest]
  implicit def toHttpRes(res: ServletResponse) = res.asInstanceOf[HttpServletResponse]

  implicit class RichResponse(val res: HttpServletResponse) extends AnyVal {
    def setHeader(etag: ETag): Unit = etag.setTo(res)
    def addHeader(etag: ETag): Unit = etag.addTo(res)
  }
  implicit class RichRequest(val req: HttpServletRequest) extends AnyVal {
    def IfNoneMatch() = ETag.IfNoneMatch(req)
    def IfMatch() = ETag.IfMatch(req)
    def Accept() = AcceptHeader(req)
    def IfModifiedSince() = req.getDateHeader(HttpHeaders.IfModifiedSince) match {
      case -1 ⇒ None
      case ims ⇒ Some(ims)
    }
    def IfModifiedSince(lastModified: Long): Boolean = {
      IfModifiedSince match {
        case None ⇒ true
        case Some(modSince) ⇒
          val last = lastModified / 1000L
          val since = modSince / 1000L
          last != since
      }
    }
    def IfMatch(etag: ⇒ ETag): Boolean = {
      IfMatch match {
        case None ⇒ false
        case Some(reqETag) ⇒ reqETag == etag
      }
    }
    def IfNoneMatch(etag: ⇒ ETag): Boolean = {
      IfNoneMatch match {
        case None ⇒ false
        case Some(reqETag) ⇒ reqETag == etag
      }
    }
    def Referer(): Option[String] = req.getHeader(HttpHeaders.Referer) match {
      case "" => None
      case r => Option(r)
    }
    def userLocales: List[Locale] = {
      import collection.JavaConverters._
      req.getLocales().asScala.toList
    }
    def userAgent: Option[String] = req.getHeader(HttpHeaders.UserAgent) match {
      case "" => None
      case ua => Option(ua)
    }
    def remoteAddr: InetAddress = InetAddress.getByName(req.getRemoteAddr)
    def servletPathInfo: String = {
      val pathInfo = req.getPathInfo match {
        case null => ""
        case info => info
      }
      req.getServletPath concat pathInfo
    }

  }

}
