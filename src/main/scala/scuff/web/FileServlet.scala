package scuff.web

import java.net.URL
import scala.util.control.NonFatal
import scuff.Document

import javax.servlet._
import http._
import HttpHeaders._

/**
 * Serve files from local file system.
 */
abstract class FileServlet extends HttpServlet {

  protected def toDocument(path: String, url: URL): Document

  /** Optional resource class loader. */
  protected def resourceClassLoader: ClassLoader = null

  /**
   * Max age, in seconds.
   * @param req The HTTP request.
   * Passed for querying, in case max-age depends on the request.
   */
  protected def maxAge(req: HttpServletRequest): Int

  private def respond(req: HttpServletRequest, res: HttpServletResponse): Unit = {
    req.getResource(resourceClassLoader) match {
      case None => res.setStatus(HttpServletResponse.SC_NOT_FOUND)
      case Some(Resource(url, lastModified)) =>
        if (req.IfModifiedSince(lastModified)) {
          res.setDateHeader(LastModified, lastModified)
          res setMaxAge maxAge(req)
          val doc = toDocument(req.servletPathInfo, url)
          res setCharacterEncoding doc.encoding
          res setContentType doc.mimeType
          doc dump res.getWriter
          res setStatus HttpServletResponse.SC_OK
        } else {
          res setStatus HttpServletResponse.SC_NOT_MODIFIED
        }
    }
  }

  override def doGet(req: HttpServletRequest, res: HttpServletResponse): Unit =
    try respond(req, res) catch {
      case NonFatal(cause) =>
        log(s"Failed to GET: ${req.servletPathInfo}", cause)
        res.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, cause.getMessage)
    }

}
