package scuff

import java.net.{ InetAddress, URL }
import java.util.Locale

import scala.collection.JavaConverters._
import scala.language.implicitConversions

import javax.servlet.{ ServletRequest, ServletResponse }
import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }
import scala.util.{ Try, Success, Failure }

package web {
  case class Resource(url: URL, lastModified: Long)
}

package object web {

  private[this] val JarSplit = "!".r.pattern

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
    def sendPermanentRedirect(url: CharSequence): Unit = {
      res.setStatus(HttpServletResponse.SC_MOVED_PERMANENTLY)
      res.setHeader(HttpHeaders.Location, url.toString)
      res.flushBuffer()
    }
    def setCookie[T](value: T)(implicit req: HttpServletRequest, cm: CookieMonster[T]): Unit = {
      cm.set(res, value)
    }
    def removeCookie[T](cm: CookieMonster[T]): Unit = {
      cm.remove(res)
    }
  }
  implicit class ScuffRequest(private val req: HttpServletRequest) extends AnyVal {
    def isLocalhost: Boolean = req.getRemoteHost match {
      case "localhost" | "127.0.0.1" | "0:0:0:0:0:0:0:1" | "::1" => true
      case _ => false
    }
    def set[T](attr: Attribute[T], value: T): Unit = attr.set(req, value)
    def get[T](attr: Attribute[T]): Option[T] = attr.get(req)
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

    private def toFile(url: URL): java.io.File =
      url.getProtocol match {
        case "file" => new java.io.File(url.getFile)
        case "jar" =>
          val Array(jar, _*) = JarSplit.split(url.getFile)
          toFile(new URL(jar))
        case _ => sys.error(s"Cannot handle $url")
      }

    def getResource: Option[Resource] = getResource("")
    def getResource(prefixPath: String): Option[Resource] = {
      val urlFile =
        Option(req.getServletContext.getResource(s"$prefixPath$servletPathInfo"))
          .map(url => url -> toFile(url))
      urlFile.filter(_._2.exists) map {
        case (url, file) => new Resource(url, (file.lastModified / 1000) * 1000)
      }
    }
    def IfNoneMatch = ETag.IfNoneMatch(req)
    def IfMatch = ETag.IfMatch(req)
    def Accept = AcceptHeader(req)
    def IfModifiedSince() = Try(req.getDateHeader(HttpHeaders.IfModifiedSince)) match {
      case Success(-1L) | Failure(_) => None
      case Success(ims) => Some(ims)
    }
    def IfModifiedSince(lastModified: Long): Boolean = {
      IfModifiedSince().forall { modSince =>
        val last = lastModified / 1000L
        val since = modSince / 1000L
        last != since
      }
    }
    def IfMatch(matchTag: ETag): Boolean = IfMatch.exists(etag => etag.tag == "*" || etag == matchTag)
    def IfNoneMatch(matchTag: ETag): Boolean = IfNoneMatch.exists(etag => etag.tag == "*" || etag == matchTag)
    def Referer: Option[String] = req.getHeader(HttpHeaders.Referer).optional
    def Expect: Option[String] = req.getHeader(HttpHeaders.Expect).optional
    def userLocales: List[Locale] = req.getLocales().asScala.toList
    def userAgent: Option[String] = req.getHeader(HttpHeaders.UserAgent).optional
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
