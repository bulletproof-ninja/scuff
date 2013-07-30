package scuff.js

import scuff.js._

import org.mozilla.javascript._

import java.io.{ Reader, InputStreamReader, BufferedReader }

object CoffeeScriptCompiler {

  sealed abstract class Use(val directive: String)
  object Use {
    case object Strict extends Use("\"use strict\";\n")
    case object ASM extends Use("\"use asm\";\n")
  }

  private val coffeeScriptCodeVarName = "coffeeScriptCode"

  def apply(options: (Symbol, Any)*): CoffeeScriptCompiler = apply(Some(Use.Strict), false, options: _*)
  def apply(iced: Boolean, options: (Symbol, Any)*): CoffeeScriptCompiler = apply(None, iced, options: _*)
  def apply(useDirective: Use, options: (Symbol, Any)*): CoffeeScriptCompiler = apply(Some(useDirective), false, options: _*)
  def apply(useDirective: Use, iced: Boolean, options: (Symbol, Any)*): CoffeeScriptCompiler = apply(Some(useDirective), iced, options: _*)
  def apply(useDirective: Option[Use], iced: Boolean, options: (Symbol, Any)*): CoffeeScriptCompiler = {
    val reader = {
      val src = if (iced) "/META-INF/script/iced-coffee-script.js" else "/META-INF/script/coffee-script.js"
      Option(getClass().getResourceAsStream(src)) match {
        case Some(stream) ⇒ new InputStreamReader(stream, "UTF-8")
        case None ⇒ throw new IllegalStateException("Cannot find compiler script in classpath: " + src)
      }

    }
    this.apply(reader, useDirective, options: _*)
  }
  def apply(coffeeScriptCompilerSource: Reader, useDirective: Option[Use], options: (Symbol, Any)*): CoffeeScriptCompiler = {
    new CoffeeScriptCompiler(coffeeScriptCompilerSource, useDirective.map(_.directive).getOrElse(""), options)
  }

}

class CoffeeScriptCompiler private (compilerSource: Reader, useDirective: String, options: Seq[(Symbol, Any)]) {
  import CoffeeScriptCompiler._

  private val defaultOptions = options.toMap

  private def jsCompile(otherOptions: Seq[(Symbol, Any)]) = {
    val options = defaultOptions ++ otherOptions
    "CoffeeScript.compile(%s, %s);".format(coffeeScriptCodeVarName, toJavascript(options.toSeq))
  }

  private val globalScope = try {
    withContext { context ⇒
      val globalScope = context.initStandardObjects()
      context.evaluateReader(globalScope, compilerSource, "coffee-script.js", 0, null)
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
