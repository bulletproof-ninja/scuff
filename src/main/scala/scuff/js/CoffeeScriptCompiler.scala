package scuff.js

import java.io.{ InputStreamReader, Reader }

import javax.script.{ Compilable, ScriptEngine }

object CoffeeScriptCompiler {

  sealed abstract class Version(compilerPath: String) {
    def compiler(): Reader = getClass().getResourceAsStream(compilerPath) match {
      case null ⇒ sys.error("Cannot find compiler script in classpath: " + compilerPath)
      case stream ⇒ new InputStreamReader(stream, "UTF-8")
    }
  }
  case object Version {
    case object Original extends Version("/META-INF/script/coffee-script.js")
    case object Iced extends Version("/META-INF/script/iced-coffee-script.js")
    case object Redux extends Version("/META-INF/script/CoffeeScriptRedux.js")
  }

  sealed abstract class Use(val directive: String)
  object Use {
    case object Strict extends Use("\"use strict\";\n")
    case object ASM extends Use("\"use asm\";\n")
  }

  case class Config(options: Map[Symbol, Any] = Map.empty, newEngine: () => ScriptEngine = newJavascriptEngine, useDirective: Use = null, compiler: () ⇒ Reader = Version.Original.compiler)
  private val coffeeScriptCodeVarName = "coffeeScriptCode"

}

/**
 * NOTICE: An instance of this class IS NOT thread-safe.
 */
class CoffeeScriptCompiler(config: CoffeeScriptCompiler.Config = new CoffeeScriptCompiler.Config) {
  import CoffeeScriptCompiler._

  private[this] val useDirective = Option(config.useDirective).map(_.directive).getOrElse("")

  private def jsCompile() = {
    s"CoffeeScript.compile($coffeeScriptCodeVarName, ${toJavascript(config.options.toSeq)});"
  }

  private val coffeeCompiler = {
    val compilerSource = config.compiler()
    try {
      config.newEngine() match {
        case engine: Compilable ⇒
          val compSrc: String = compilerSource
          engine.compile(compSrc + ";\n" + jsCompile())
        case _ ⇒ sys.error(s"Cannot find Javascript engine!")
      }
    } finally {
      compilerSource.close()
    }
  }

  def compile(coffeeScriptCode: String, filename: String = ""): String = {
    val coffeeCode = useDirective concat coffeeScriptCode
    val bindings = coffeeCompiler.getEngine.createBindings()
    bindings.put(ScriptEngine.FILENAME, filename)
    bindings.put(coffeeScriptCodeVarName, coffeeCode)
    String.valueOf(coffeeCompiler.eval(bindings))
  }

}

