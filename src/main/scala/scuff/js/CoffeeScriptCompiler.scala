package scuff.js

import scuff.js._

import org.mozilla.javascript._

import java.io.{ Reader, InputStreamReader, BufferedReader }

object CoffeeScriptCompiler {

  private val coffeeScriptCodeVarName = "coffeeScriptCode"

  def apply(useStrict: Boolean, iced: Boolean, options: (Symbol, Any)*): CoffeeScriptCompiler = {
    val reader = {
      val src = if (iced) "/META-INF/script/iced-coffee-script.js" else "/META-INF/script/coffee-script.js"
      new InputStreamReader(getClass().getResourceAsStream(src), "UTF-8")
    }
    this.apply(reader, useStrict, options: _*)
  }
  def apply(coffeeScriptCompilerSource: Reader, useStrict: Boolean, options: (Symbol, Any)*): CoffeeScriptCompiler = {
    val optionString = toJavascript(options)
    new CoffeeScriptCompiler(coffeeScriptCompilerSource, useStrict, optionString)
  }

}

class CoffeeScriptCompiler private (compilerSource: Reader, useStrict: Boolean, options: String) {
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
    val code = (if (useStrict) "'use strict';\n" else "") + coffeeScriptCode
    val compileScope = context.newObject(globalScope)
    compileScope.setParentScope(globalScope)
    compileScope.put(coffeeScriptCodeVarName, compileScope, code)
    String.valueOf(context.evaluateString(compileScope, jsCompile, name, 0, null))
  }

}
