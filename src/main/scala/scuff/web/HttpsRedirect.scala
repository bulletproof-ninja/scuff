package scuff.web

import javax.servlet.http._

/**
 * Add to servlets to ensure they are
 * accessed through HTTPS rather than HTTP.
 */
trait HttpsRedirect extends HttpServlet {
  /** Should return client protocol in lower case, no version information. */
  protected def getProtocol(req: HttpServletRequest): String = req.getClientScheme

  /** Return `true` to enable redirect for this request. */
  protected def isHttpsRedirectEnabled(req: HttpServletRequest): Boolean

  override def service(req: HttpServletRequest, res: HttpServletResponse): Unit = {
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
