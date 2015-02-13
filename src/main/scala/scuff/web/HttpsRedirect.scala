package scuff.web

import javax.servlet.http._
import scuff.web._
import collection.JavaConversions._

/**
 * Add to servlets to ensure they are
 * accessed through HTTPS rather than HTTP.
 */
trait HttpsRedirect extends HttpServlet {
  /** Should return client protocol in lower case, no version information. */
  protected def getProtocol(req: HttpServletRequest): String = req.getClientScheme

  /** Must be `true` to enable this trait. */
  protected def isHttpsRedirectEnabled(req: HttpServletRequest): Boolean

  override def service(req: HttpServletRequest, res: HttpServletResponse) {
    if (isHttpsRedirectEnabled(req) && req.getMethod == "GET" && getProtocol(req) == "http") {
      val url = req.getRequestURL()
      url.replace(0, 4, "https")
      req.getQueryString match {
        case null => // Ignore
        case qryStr => url.append('?').append(qryStr)
      }
      res sendPermanentRedirect url
    } else {
      super.service(req, res)
    }
  }
}
