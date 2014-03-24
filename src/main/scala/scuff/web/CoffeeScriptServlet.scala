package scuff.web

import javax.servlet._
import http._
import HttpHeaders._
import scuff.js._
import java.net.URL
import CoffeeScriptCompiler._
import scuff.ResourcePool
import scala.util.Try
import concurrent.duration._

private object CoffeeScriptServlet {
  import CoffeeScriptCompiler._
  def DefaultConfig(engineName: String) = new Config(
    options = Map('bare -> false), engineName = engineName,
    useDirective = Use.Strict)
  def IcedConfig(engineName: String) = new Config(
    options = Map('bare -> false), engineName = engineName,
    useDirective = Use.Strict, compiler = Version.Iced.compiler)
  val lastModifiedMap = new scuff.LockFreeConcurrentMap[String, Option[Long]]

}

/**
 * Perform on-the-fly conversion of CoffeeScript
 * to JavaScript.
 * <p>Use with [[scuff.web.Ice]] for Iced CoffeeScript.
 */
abstract class CoffeeScriptServlet extends HttpServlet with FileResourceLookup with HttpCaching {
  import CoffeeScriptCompiler._
  import CoffeeScriptServlet._

  /**
   * Javascript engine name. We default
   * to "rhino" because "nashorn" is slow
   * to the point of being unusable for
   * this.
   */
  protected def jsEngineName = "rhino"
  protected def newCoffeeCompiler() = new CoffeeScriptCompiler(CoffeeScriptServlet.DefaultConfig(jsEngineName))
  private[this] val compilerPool = new ResourcePool[CoffeeScriptCompiler](createCompiler) {
    // Don't discard compiler on exception 
    override def borrow[A](thunk: CoffeeScriptCompiler ⇒ A): A = {
      val result = super.borrow { compiler ⇒
        Try(thunk(compiler))
      }
      result.get
    }
  }

  private def createCompiler = {
    val started = System.currentTimeMillis()
    val comp = newCoffeeCompiler()
    val dur = System.currentTimeMillis() - started
    log(s"Initialized ${comp.getClass.getName} in $dur ms.")
    comp
  }
  private def onCompilerTimeout(comp: CoffeeScriptCompiler) {
    log(s"${comp.getClass.getName} instance removed from pool. ${compilerPool.size} remaining.")
  }

  override def init() {
    super.init()
    compilerPool.startPruning(120.minutes, onCompilerTimeout)
  }

  protected def coffeeCompilation(coffeeScript: String, filename: String): String =
    compilerPool.borrow(_.compile(coffeeScript, filename))

  private def compile(path: String, url: URL): String = {
    val script = url.openStream()
    try {
      coffeeCompilation(script, path)
    } finally {
      script.close()
    }
  }

  /**
   * Max age, in seconds.
   * @param req The HTTP request.
   * Passed for querying, in case max-age depends on the request.
   * Ignore if max-age is fixed.
   */
  protected def maxAge(req: HttpServletRequest): Int

  private def respond(req: HttpServletRequest, res: HttpServletResponse) {
    findResource(req) match {
      case None ⇒ res.setStatus(HttpServletResponse.SC_NOT_FOUND)
      case Some((url, lastModified)) ⇒
        if (req.IfModifiedSince(lastModified)) {
          val js = compile(req.getServletPath, url)
          res.setDateHeader(LastModified, lastModified)
          res.setHeader(CacheControl, "max-age=" + maxAge(req))
          res.setCharacterEncoding("UTF-8")
          res.setContentType("text/javascript")
          res.getWriter.print(js)
          res.setStatus(HttpServletResponse.SC_OK)
        } else {
          res.setStatus(HttpServletResponse.SC_NOT_MODIFIED)
        }
    }
  }

  override def doGet(req: HttpServletRequest, res: HttpServletResponse) {
    try {
      respond(req, res)
    } catch {
      case e: Exception ⇒
        log("Failed to compile: " + req.getServletPath, e)
        res.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR)
    }
  }

  protected def isProduction: Boolean

  def fetchLastModified(req: HttpServletRequest) =
    if (isProduction) {
      lastModifiedMap.get(req.getServletPath).getOrElse {
        val last = findResource(req).map(_._2)
        lastModifiedMap.put(req.getServletPath, last)
        last
      }
    } else {
      findResource(req).map(_._2)
    }

  def makeCacheKey(req: HttpServletRequest) = Some(req.getServletPath)

}

trait Ice { self: CoffeeScriptServlet ⇒
  final override def newCoffeeCompiler() = new CoffeeScriptCompiler(CoffeeScriptServlet.IcedConfig(jsEngineName))
}

//trait Redux { self: CoffeeScriptServlet ⇒
//  import CoffeeScriptCompiler.Use
//  final override def newCoffeeCompiler() = new CoffeeScriptCompiler()
//}
