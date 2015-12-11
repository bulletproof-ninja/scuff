package scuff

import java.net.{InetAddress, URL}
import java.util.Locale

import scala.collection.JavaConverters._
import scala.language.implicitConversions

import javax.servlet.{ServletRequest, ServletResponse}
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

package web {
  case class Resource(url: URL, lastModified: Long)
}

package object web {
  private val RFC822Pool = new ThreadLocal[java.text.SimpleDateFormat] {
    override def initialValue = new java.text.SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss Z", java.util.Locale.US)
  }
  /** ThreadLocal safe date parser. */
  def RFC822 = RFC822Pool.get()

  @inline
  implicit def toHttpReq(req: ServletRequest) = req.asInstanceOf[HttpServletRequest]
  @inline
  implicit def toHttpRes(res: ServletResponse) = res.asInstanceOf[HttpServletResponse]

  implicit class ScuffResponse(private val res: HttpServletResponse) extends AnyVal {
    def setHeader(etag: ETag): HttpServletResponse = { etag.setTo(res); res }
    def addHeader(etag: ETag): HttpServletResponse = { etag.addTo(res); res }
    def setMaxAge(seconds: Int): HttpServletResponse = {
      res.setHeader(HttpHeaders.CacheControl, s"max-age=$seconds")
      res
    }
    def setLastModified(date: Long): HttpServletResponse = {
      res.setDateHeader(HttpHeaders.LastModified, date)
      res
    }
    def sendPermanentRedirect(url: CharSequence) {
      res.setStatus(HttpServletResponse.SC_MOVED_PERMANENTLY)
      res.setHeader(HttpHeaders.Location, url.toString)
      res.flushBuffer()
    }
    def setCookie[T](value: T)(implicit req: HttpServletRequest, cm: CookieMonster[T]) {
      cm.set(res, value)
    }
  }
  implicit class ScuffRequest(private val req: HttpServletRequest) extends AnyVal {
    def getClientScheme: String = {
      val scheme = req.getHeader("X-Forwarded-Proto") match {
        case null => req.getHeader("X-Forwarded-Protocol")
        case fwd => fwd
      }
      scheme match {
        case null => req.getScheme
        case scheme => scheme
      }
    }
    def getClientIP: String = req.getHeader("X-Forwarded-For") match {
      case null => req.getRemoteAddr
      case addr => addr
    }
    def getClientHost: String = req.getHeader("X-Forwarded-Host") match {
      case null => req.getServerName
      case host =>
        host.lastIndexOf(':') match {
          case -1 => host
          case portIdx => host.substring(0, portIdx)
        }
    }

    def getResource: Option[Resource] = {
      req.getServletContext.getResource(servletPathInfo) match {
        case null => None
        case url =>
          val file = new java.io.File(url.toURI)
          if (file.exists) {
            Some(new Resource(url, (file.lastModified / 1000) * 1000))
          } else {
            None
          }
      }
    }
    def IfNoneMatch() = ETag.IfNoneMatch(req)
    def IfMatch() = ETag.IfMatch(req)
    def Accept() = AcceptHeader(req)
    def IfModifiedSince() = req.getDateHeader(HttpHeaders.IfModifiedSince) match {
      case -1 => None
      case ims => Some(ims)
    }
    def IfModifiedSince(lastModified: Long): Boolean = {
      IfModifiedSince match {
        case None => true
        case Some(modSince) =>
          val last = lastModified / 1000L
          val since = modSince / 1000L
          last != since
      }
    }
    def IfMatch(etag: => ETag): Boolean = {
      IfMatch match {
        case None => false
        case Some(reqETag) => reqETag == etag
      }
    }
    def IfNoneMatch(etag: => ETag): Boolean = {
      IfNoneMatch match {
        case None => false
        case Some(reqETag) => reqETag == etag
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
