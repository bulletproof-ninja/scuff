package scuff.js

import scuff.js._

import org.mozilla.javascript._

import java.io.{ Reader, InputStreamReader, BufferedReader }

object CoffeeScriptCompiler {

  sealed trait Fork
  case object Fork {
    case object Original extends Fork
    case object Iced extends Fork
    case object Redux extends Fork
  }

  sealed abstract class Use(val directive: String)
  object Use {
    case object Strict extends Use("\"use strict\";\n")
    case object ASM extends Use("\"use asm\";\n")
  }

  private val coffeeScriptCodeVarName = "coffeeScriptCode"

  def apply(options: (Symbol, Any)*): CoffeeScriptCompiler = apply(None, Fork.Original, options: _*)
  def apply(fork: Fork, options: (Symbol, Any)*): CoffeeScriptCompiler = apply(None, fork, options: _*)
  def apply(useDirective: Use, options: (Symbol, Any)*): CoffeeScriptCompiler = apply(Some(useDirective), Fork.Original, options: _*)
  def apply(useDirective: Use, fork: Fork, options: (Symbol, Any)*): CoffeeScriptCompiler = apply(Some(useDirective), fork, options: _*)
  def apply(useDirective: Option[Use], fork: Fork, options: (Symbol, Any)*): CoffeeScriptCompiler = {
    val (src, reader) = {
      val src = fork match {
        case Fork.Original ⇒ "/META-INF/script/coffee-script.js"
        case Fork.Iced ⇒ "/META-INF/script/iced-coffee-script.js"
        case Fork.Redux ⇒ "/META-INF/script/CoffeeScriptRedux.js"
      }
      Option(getClass().getResourceAsStream(src)) match {
        case Some(stream) ⇒ src.substring(src.lastIndexOf("/")+1) -> new InputStreamReader(stream, "UTF-8")
        case None ⇒ throw new IllegalStateException("Cannot find compiler script in classpath: " + src)
      }

    }
    this.apply(src, reader, useDirective, options: _*)
  }
  def apply(coffeeScriptCompilerName: String, coffeeScriptCompiler: Reader, useDirective: Option[Use], options: (Symbol, Any)*): CoffeeScriptCompiler = {
    new CoffeeScriptCompiler(coffeeScriptCompilerName, coffeeScriptCompiler, useDirective.map(_.directive).getOrElse(""), options)
  }

}

class CoffeeScriptCompiler private (compilerName: String, compilerSource: Reader, useDirective: String, options: Seq[(Symbol, Any)]) {
  import CoffeeScriptCompiler._

  private val defaultOptions = options.toMap

  private def jsCompile(otherOptions: Seq[(Symbol, Any)]) = {
    val options = defaultOptions ++ otherOptions
    "CoffeeScript.compile(%s, %s);".format(coffeeScriptCodeVarName, toJavascript(options.toSeq))
  }

  private val globalScope = try {
    withContext { context ⇒
      val globalScope = context.initStandardObjects()
      context.evaluateReader(globalScope, compilerSource, compilerName, 0, null)
      globalScope
    }
  } finally {
    compilerSource.close()
  }

  def compile(coffeeScriptCode: String, name: String = "", options: Seq[(Symbol, Any)] = Seq.empty): String = withContext { context ⇒
    val code = useDirective concat coffeeScriptCode
    val compileScope = context.newObject(globalScope)
    compileScope.setParentScope(globalScope)
    compileScope.put(coffeeScriptCodeVarName, compileScope, code)
    String.valueOf(context.evaluateString(compileScope, jsCompile(options), name, 0, null))
  }

}
