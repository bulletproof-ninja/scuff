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
import scuff.Document
import java.io.Writer

object CoffeeScriptServlet {

  import CoffeeScriptCompiler._
  def LegacyConfig(engineCtor: () => ScriptEngine) = new Config(
    version = Version.Legacy,
    options = Map(bare -> false), newEngine = engineCtor,
    useDirective = Use.Strict)
  def CS2Config(engineCtor: () => ScriptEngine) = new Config(
    version = Version.CS2,
    options = Map(bare -> false), newEngine = engineCtor,
    useDirective = Use.Strict, compiler = Version.CS2.compiler _)
  def IcedConfig(engineCtor: () => ScriptEngine) = new Config(
    version = Version.Iced,
    options = Map(bare -> false, runtime -> "window"), newEngine = engineCtor,
    useDirective = Use.Strict, compiler = Version.Iced.compiler _)
}

/**
 * Perform on-the-fly conversion of CoffeeScript to JavaScript.
 *
 * Use with [[scuff.web.Ice]] for Iced CoffeeScript.
 */
abstract class CoffeeScriptServlet
extends FileServlet {

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
    new UnboundedResourcePool(createCompiler, minResources = 0, description = engineName)
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
    super.destroy()
  }

  protected def coffeeCompilation(coffeeScript: String, filename: String): String =
    compilerPool.use(_.compile(coffeeScript, filename))

  private def compile(path: String, url: URL): String = {
    val started = System.currentTimeMillis()
    val script = url.openStream()
    try coffeeCompilation(script, path) finally {
      script.close()
      val dur = System.currentTimeMillis() - started
      log(s"Compiled $path in $dur ms.")
    }
  }

  protected def toDocument(path: String, url: URL): Document =
    new Document {

      def dump(out: Writer): Unit =
        out write compile(path, url)

      def mimeType: String = "application/javascript"
      def encoding: String = "UTF-8"
    }

}

trait Ice { self: CoffeeScriptServlet =>
  final override def newCoffeeCompiler() = new CoffeeScriptCompiler(CoffeeScriptServlet.IcedConfig(newJavascriptEngine _))
}
