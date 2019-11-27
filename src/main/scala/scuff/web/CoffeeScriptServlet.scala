package scuff.web

import javax.servlet._
import http._
import HttpHeaders._
import scuff.js._
import java.net.URL
import scuff.concurrent.ResourcePool
import scala.concurrent.duration._
import javax.script._
import java.util.concurrent.ScheduledFuture
import scala.util.control.NonFatal
import scuff.concurrent.Threads
import scuff.concurrent.UnboundedResourcePool
import java.util.concurrent.ScheduledExecutorService

object CoffeeScriptServlet {
  import CoffeeScriptCompiler._
  def LegacyConfig(engineCtor: () => ScriptEngine) = new Config(
    version = Version.Legacy,
    options = Map('bare -> false), newEngine = engineCtor,
    useDirective = Use.Strict)
  def CS2Config(engineCtor: () => ScriptEngine) = new Config(
    version = Version.CS2,
    options = Map('bare -> false), newEngine = engineCtor,
    useDirective = Use.Strict, compiler = Version.CS2.compiler _)
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

  /** Number of compilers to initialize at startup. */
  protected def initCompilers = 0
  protected def engineName = "javascript"
  protected def newJavascriptEngine() = ScriptEngineMgr.getEngineByName(engineName)
  protected def newCoffeeCompiler() = new CoffeeScriptCompiler(CoffeeScriptServlet.CS2Config(newJavascriptEngine _))
  private[this] val compilerPool = {
    implicit val lifecycle = ResourcePool.onEviction(onCompilerTimeout) {
      case NonFatal(_) => false
    }
    ResourcePool(createCompiler, minResources = 0, description = engineName)
  }

  private def createCompiler = {
    val started = System.currentTimeMillis()
    val comp = newCoffeeCompiler()
    val dur = System.currentTimeMillis() - started
    log(s"Initialized $comp in $dur ms.")
    comp
  }
  private def onCompilerTimeout(comp: CoffeeScriptCompiler): Unit = {
    log(s"$comp instance removed from pool. ${compilerPool.availableCount} available.")
  }

  /** Compiler pool eviction scheduler. */
  protected def evictionScheduler: Option[ScheduledExecutorService]

  @volatile private[this] var eviction: Option[ScheduledFuture[_]] = None

  override def init(): Unit = {
    super.init()
    val initCompilers = this.initCompilers
    if (initCompilers > 0) {
      Threads.onBlockingThread(s"$engineName-initializer") {
        for (_ <- 1 to initCompilers) {
          compilerPool push createCompiler
        }
      }.failed.foreach(th => log("Failure during compiler initialization", th))(Threads.PiggyBack)
    }
    this.eviction = evictionScheduler.map(compilerPool.startEviction(120.minutes, _))
  }

  override def destroy(): Unit = {
    eviction.foreach(_.cancel(true))
    compilerPool.drain()
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

  private def respond(req: HttpServletRequest, res: HttpServletResponse): Unit = {
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

  override def doGet(req: HttpServletRequest, res: HttpServletResponse): Unit = {
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
