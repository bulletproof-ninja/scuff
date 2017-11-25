package scuff.js

import java.io.{ InputStreamReader, Reader }

import javax.script.{ Compilable, ScriptEngine }

object CoffeeScriptCompiler {

  sealed abstract class Version(compilerPath: String) {
    def compiler(): Reader = getClass().getResourceAsStream(compilerPath) match {
      case null => sys.error("Cannot find compiler script in classpath: " + compilerPath)
      case stream => new InputStreamReader(stream, "UTF-8")
    }
    def defaultOptions: Map[Symbol, Any] = Map.empty
    def polyfills: List[String] = Nil
  }
  case object Version {
    case object Original extends Version("/META-INF/script/coffee-script.js")
    case object Iced extends Version("/META-INF/script/iced-coffee-script.js") {
      override val defaultOptions = Map('runtime -> "none")
    }
    case object Coffeescript2 extends Version("/META-INF/script/coffeescript2.js") {
      override def polyfills: List[String] = List(Polyfills.Object_assign)
    }

    @deprecated("Never that useful", since = "now")
    case object Redux extends Version("/META-INF/script/CoffeeScriptRedux.js")

  }

  sealed abstract class Use(val directive: String)
  object Use {
    case object Strict extends Use("\"use strict\";\n")
    case object ASM extends Use("\"use asm\";\n")
  }

  case class Config(version: Version = Version.Original, options: Map[Symbol, Any] = Map.empty, newEngine: () => ScriptEngine = newJavascriptEngine _, useDirective: Use = null, compiler: () => Reader = () => null)
  private val coffeeScriptCodeVarName = "coffeeScriptCode"

  object Polyfills {
    /** @see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/assign#Polyfill */
    final val Object_assign = """
if (typeof Object.assign != 'function') {
  // Must be writable: true, enumerable: false, configurable: true
  Object.defineProperty(Object, "assign", {
    value: function assign(target, varArgs) { // .length of function is 2
      'use strict';
      if (target == null) { // TypeError if undefined or null
        throw new TypeError('Cannot convert undefined or null to object');
      }

      var to = Object(target);

      for (var index = 1; index < arguments.length; index++) {
        var nextSource = arguments[index];

        if (nextSource != null) { // Skip over if undefined or null
          for (var nextKey in nextSource) {
            // Avoid bugs when hasOwnProperty is shadowed
            if (Object.prototype.hasOwnProperty.call(nextSource, nextKey)) {
              to[nextKey] = nextSource[nextKey];
            }
          }
        }
      }
      return to;
    },
    writable: true,
    configurable: true
  });
}
"""
  }

}

/**
  * NOTICE: An instance of this class IS NOT thread-safe.
  */
class CoffeeScriptCompiler(config: CoffeeScriptCompiler.Config = new CoffeeScriptCompiler.Config) {
  import CoffeeScriptCompiler._

  private[this] val useDirective = Option(config.useDirective).map(_.directive).getOrElse("")

  private def jsCompile() = {
    val options = config.version.defaultOptions ++ config.options
    s"CoffeeScript.compile($coffeeScriptCodeVarName, ${toJavascript(options.toSeq)});"
  }

  private val coffeeCompiler = {
    val compilerSource = config.compiler() match {
      case null => config.version.compiler()
      case source => source
    }
    try {
      config.newEngine() match {
        case engine: Compilable =>
          val compSrc: String = compilerSource
          val polyfills = config.version.polyfills.mkString("\n")
          engine.compile(s"$polyfills\n$compSrc;\n${jsCompile()}")
        case _ => sys.error(s"Cannot find Javascript engine!")
      }
    } finally {
      compilerSource.close()
    }
  }

  override def toString(): String = s"CoffeeScriptCompiler(${coffeeCompiler.getEngine.getClass.getName})"

  def compile(coffeeScriptCode: String, filename: String = ""): String = {
    val coffeeCode = useDirective concat coffeeScriptCode
    val bindings = coffeeCompiler.getEngine.createBindings()
    bindings.put(ScriptEngine.FILENAME, filename)
    bindings.put(coffeeScriptCodeVarName, coffeeCode)
    String.valueOf(coffeeCompiler.eval(bindings))
  }

}
