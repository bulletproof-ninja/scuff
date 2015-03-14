package scuff.js

import java.io.{ InputStreamReader, Reader }

import javax.script.{ Compilable, ScriptEngine }

object LessCompiler {

  //  sealed abstract class Version(compilerPath: String) {
  //    def compiler(): Reader = getClass().getResourceAsStream(compilerPath) match {
  //      case null => sys.error("Cannot find compiler script in classpath: " + compilerPath)
  //      case stream => new InputStreamReader(stream, "UTF-8")
  //    }
  //    def defaultOptions: Map[Symbol, Any] = Map.empty
  //  }
  //  case object Version {
  //    case object Original extends Version("/META-INF/script/coffee-script.js")
  //    case object Iced extends Version("/META-INF/script/iced-coffee-script.js") {
  //      override val defaultOptions = Map('runtime -> "none")
  //    }
  //    case object Redux extends Version("/META-INF/script/CoffeeScriptRedux.js")
  //  }
  //
  //  sealed abstract class Use(val directive: String)
  //  object Use {
  //    case object Strict extends Use("\"use strict\";\n")
  //    case object ASM extends Use("\"use asm\";\n")
  //  }

  private def defaultReader: Reader = new InputStreamReader(getClass.getResourceAsStream("/META-INF/script/less.js"), "UTF-8")

  case class Config(options: Map[Symbol, Any] = Map.empty, newEngine: () => ScriptEngine = newJavascriptEngine, compiler: () => Reader = () => defaultReader)
  private val lessCodeVarName = "lessCode"

}

/**
 * NOTICE: An instance of this class IS NOT thread-safe.
 */
class LessCompiler(config: LessCompiler.Config = new LessCompiler.Config) {
  import LessCompiler._

  private def cssCompile() = {
    s"less.render($lessCodeVarName, ${toJavascript(config.options.toSeq)});"
  }

  private val lessCompiler = {
    val compilerSource = config.compiler()
    try {
      config.newEngine() match {
        case engine: Compilable =>
          val compSrc: String = compilerSource
          engine.compile(compSrc + ";\n" + cssCompile())
        case _ => sys.error(s"Cannot find Javascript engine!")
      }
    } finally {
      compilerSource.close()
    }
  }

  override def toString(): String = s"LessCSSCompiler(${lessCompiler.getEngine.getClass.getName})"

  def compile(lessCode: String, filename: String = ""): String = {
    val bindings = lessCompiler.getEngine.createBindings()
    bindings.put(ScriptEngine.FILENAME, filename)
    bindings.put(lessCodeVarName, lessCode)
    String.valueOf(lessCompiler.eval(bindings))
  }

}

