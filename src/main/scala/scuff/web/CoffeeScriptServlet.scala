package scuff.web

import javax.servlet._
import http._
import HttpHeaders._
import scuff.js._
import java.net.URL
import scuff.concurrent.ResourcePool
import scala.util.Try
import scala.concurrent.duration._
import javax.script._
import java.util.concurrent.ScheduledFuture

private object CoffeeScriptServlet {
  import CoffeeScriptCompiler._
  def DefaultConfig(engineCtor: () => ScriptEngine) = new Config(
    version = Version.Original,
    options = Map('bare -> false), newEngine = engineCtor,
    useDirective = Use.Strict)
  def Coffeescript2Config(engineCtor: () => ScriptEngine) = new Config(
    version = Version.Coffeescript2,
    options = Map('bare -> false), newEngine = engineCtor,
    useDirective = Use.Strict, compiler = Version.Coffeescript2.compiler _)
  def IcedConfig(engineCtor: () => ScriptEngine) = new Config(
    version = Version.Iced,
    options = Map('bare -> false, 'runtime -> "window"), newEngine = engineCtor,
    useDirective = Use.Strict, compiler = Version.Iced.compiler _)
}

/**
  * Perform on-the-fly conversion of CoffeeScript to JavaScript.
  *
  * Use with [[scuff.web.Ice]] for Iced CoffeeScript.
  */
abstract class CoffeeScriptServlet extends HttpServlet {

  private lazy val ScriptEngineMgr = new ScriptEngineManager

  protected def engineName = "javascript"
  protected def newJavascriptEngine() = ScriptEngineMgr.getEngineByName(engineName)
  protected def newCoffeeCompiler() = new CoffeeScriptCompiler(CoffeeScriptServlet.DefaultConfig(newJavascriptEngine _))
  private[this] val compilerPool = new ResourcePool[CoffeeScriptCompiler](createCompiler) {
    // Don't discard compiler on exception, it still works :-)
    override def use[A](thunk: CoffeeScriptCompiler => A): A = {
      val result = super.use { compiler =>
        Try(thunk(compiler))
      }
      result.get
    }
  }

  private def createCompiler = {
    val started = System.currentTimeMillis()
    val comp = newCoffeeCompiler()
    val dur = System.currentTimeMillis() - started
    log(s"Initialized $comp in $dur ms.")
    comp
  }
  private def onCompilerTimeout(comp: CoffeeScriptCompiler) {
    log(s"$comp instance removed from pool. ${compilerPool.size} remaining.")
  }

  @volatile private var pruner: Option[ScheduledFuture[_]] = None

  override def init() {
    super.init()
    this.pruner = Some(compilerPool.startPruning(120.minutes, onCompilerTimeout))
  }

  override def destroy {
    pruner.foreach(_.cancel(true))
  }

  protected def coffeeCompilation(coffeeScript: String, filename: String): String =
    compilerPool.use(_.compile(coffeeScript, filename))

  private def compile(path: String, url: URL): String = {
    val started = System.currentTimeMillis()
    val script = url.openStream()
    try {
      coffeeCompilation(script, path)
    } finally {
      script.close()
      val dur = System.currentTimeMillis() - started
      log(s"Compiled $path in $dur ms.")
    }
  }

  /**
    * Max age, in seconds.
    * @param req The HTTP request.
    * Passed for querying, in case max-age depends on the request.
    */
  protected def maxAge(req: HttpServletRequest): Int

  private def respond(req: HttpServletRequest, res: HttpServletResponse) {
    req.getResource match {
      case None => res.setStatus(HttpServletResponse.SC_NOT_FOUND)
      case Some(Resource(url, lastModified)) =>
        if (req.IfModifiedSince(lastModified)) {
          val js = compile(req.servletPathInfo, url)
          res.setDateHeader(LastModified, lastModified)
          res.setMaxAge(maxAge(req))
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
      case e: Exception =>
        log(s"Failed to compile: ${req.servletPathInfo}", e)
        res.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e.getMessage)
    }
  }

}

trait Ice { self: CoffeeScriptServlet =>
  final override def newCoffeeCompiler() = new CoffeeScriptCompiler(CoffeeScriptServlet.IcedConfig(newJavascriptEngine _))
}
