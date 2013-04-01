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

  def apply(options: (Symbol, Any)*): CoffeeScriptCompiler = apply(None, false, options: _*)
  def apply(iced: Boolean, options: (Symbol, Any)*): CoffeeScriptCompiler = apply(None, iced, options: _*)
  def apply(useDirective: Use, options: (Symbol, Any)*): CoffeeScriptCompiler = apply(Some(useDirective), false, options: _*)
  def apply(useDirective: Use, iced: Boolean, options: (Symbol, Any)*): CoffeeScriptCompiler = apply(Some(useDirective), iced, options: _*)
  def apply(useDirective: Option[Use], iced: Boolean, options: (Symbol, Any)*): CoffeeScriptCompiler = {
    val reader = {
      val src = if (iced) "/META-INF/script/iced-coffee-script.js" else "/META-INF/script/coffee-script.js"
      new InputStreamReader(getClass().getResourceAsStream(src), "UTF-8")
    }
    this.apply(reader, useDirective, options: _*)
  }
  def apply(coffeeScriptCompilerSource: Reader, useDirective: Option[Use], options: (Symbol, Any)*): CoffeeScriptCompiler = {
    val optionString = toJavascript(options)
    new CoffeeScriptCompiler(coffeeScriptCompilerSource, useDirective.map(_.directive).getOrElse(""), optionString)
  }

}

class CoffeeScriptCompiler private (compilerSource: Reader, useDirective: String, options: String) {
  import CoffeeScriptCompiler._

  private val jsCompile = "CoffeeScript.compile(%s, %s);".format(coffeeScriptCodeVarName, options)

  private val globalScope = try {
    withContext { context ⇒
      val globalScope = context.initStandardObjects()
      context.evaluateReader(globalScope, compilerSource, "coffee-script.js", 0, null)
      globalScope
    }
  } finally {
    compilerSource.close()
  }

  def compile(coffeeScriptCode: String, name: String = ""): String = withContext { context ⇒
    val code = useDirective concat coffeeScriptCode
    val compileScope = context.newObject(globalScope)
    compileScope.setParentScope(globalScope)
    compileScope.put(coffeeScriptCodeVarName, compileScope, code)
    String.valueOf(context.evaluateString(compileScope, jsCompile, name, 0, null))
  }

}
