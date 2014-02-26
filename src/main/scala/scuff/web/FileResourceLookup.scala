package scuff.web

import javax.servlet.http.HttpServlet
import java.net.URL
import javax.servlet.http.HttpServletRequest

trait FileResourceLookup { this: HttpServlet ⇒
  protected def findResource(req: HttpServletRequest): Option[(URL, Long)] = {
    getServletContext.getResource(req.getServletPath) match {
      case null ⇒ None
      case url ⇒
        val file = new java.io.File(url.toURI)
        if (file.exists) {
          Some(url -> file.lastModified / 1000 * 1000)
        } else {
          None
        }
    }
  }

}