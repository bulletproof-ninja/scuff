package scuff.web

import javax.servlet._
import http._

import scuff.js._

/**
  * Perform on-the-fly conversion of CoffeeScript
  * to JavaScript.
  */
class CoffeeScriptServlet extends HttpServlet {
  import CoffeeScriptCompiler.Use

  protected def newCoffeeCompiler() = CoffeeScriptCompiler(Use.Strict, false, 'bare -> false)

  protected def coffeeCompilation(coffeeScript: String, filename: String): String = newCoffeeCompiler().compile(coffeeScript, filename)

  private def compile(path: String): Option[(String, Long)] = {
    getServletContext.getResource(path) match {
      case null ⇒ None
      case url ⇒
        val file = new java.io.File(url.toURI)
        if (file.exists) {
          val script = url.openStream()
          try {
            Some(coffeeCompilation(script, path) -> file.lastModified)
          } finally {
            script.close()
          }
        } else {
          None
        }
    }
  }

  override def doGet(req: HttpServletRequest, res: HttpServletResponse) {
    try {
      compile(req.getServletPath) match {
        case Some((js, lastMod)) ⇒
          res.setDateHeader("Last-Modified", lastMod)
          res.setCharacterEncoding("UTF-8")
          res.setContentType("text/javascript")
          res.getWriter.print(js)
        case None ⇒ res.setStatus(HttpServletResponse.SC_NOT_FOUND)
      }
    } catch {
      case e: Exception ⇒
        res.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR)
        val writer = res.getWriter
        writer.println("Failed to compile: " + req.getServletPath)
        e.printStackTrace(writer)
    }
  }

}

trait Ice { self: CoffeeScriptServlet ⇒
  import CoffeeScriptCompiler.Use
  final override def newCoffeeCompiler() = CoffeeScriptCompiler(Use.Strict, true, 'bare -> false)
}
