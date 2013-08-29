package scuff.js

import java.io._
import com.google.javascript.jscomp._
import java.util.Arrays.asList
import java.util.Collections.emptyList

object ClosureCompiler {

  final val Encoding = "UTF-8"

  private val DefaultOptions = defaultOptions
  def defaultOptions = {
    val opts = new CompilerOptions
    opts.aggressiveVarCheck = CheckLevel.ERROR
    opts.checkSuspiciousCode = true
    opts.checkControlStructures = true
    opts.foldConstants = true
    opts.checkTypes = true
    opts.reportMissingOverride = CheckLevel.ERROR
    opts.checkUnreachableCode = CheckLevel.ERROR
    opts.optimizeReturns = true
    opts.variableRenaming = VariableRenamingPolicy.ALL
    opts.markNoSideEffectCalls = true
    opts.propertyRenaming = PropertyRenamingPolicy.OFF
    opts.inlineConstantVars = true
    opts.inlineFunctions = true
    opts.inlineLocalFunctions = true
    opts.aliasExternals = true
    opts.collapseProperties = true
    opts.devirtualizePrototypeMethods = true
    opts.moveFunctionDeclarations = true
    opts.aliasAllStrings = true
    opts.deadAssignmentElimination = true
    opts.setSmartNameRemoval(true)
    opts.setLanguageIn(CompilerOptions.LanguageMode.ECMASCRIPT5_STRICT)
    opts.setLanguageOut(CompilerOptions.LanguageMode.ECMASCRIPT5_STRICT)
    opts.setOutputCharset(Encoding)
    opts
  }

  def compile(code: String, fileName: String = "N/A", options: CompilerOptions = DefaultOptions): String = {
    import java.util.Collections._
    import java.util.Arrays._

    val err = new ByteArrayOutputStream
    val compiler = new Compiler(new PrintStream(err, true))
    compiler.disableThreads()
    val result = compiler.compile(emptyList[SourceFile](), asList(SourceFile.fromCode(fileName, code)), options)
    if (result.success) {
      compiler.toSource
    } else {
      err.toString
    }
  }

}
