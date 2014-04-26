package scuff.js

import java.io._
import com.google.javascript.jscomp._
import java.util.Arrays.asList
import java.util.Collections.emptyList

object ClosureCompiler {

  final val Encoding = "UTF-8"

  def DefaultOptions = {
    val options = new CompilerOptions
    CompilationLevel.SIMPLE_OPTIMIZATIONS.setOptionsForCompilationLevel(options)
    options.setLanguageOut(CompilerOptions.LanguageMode.ECMASCRIPT5_STRICT)
    options.setOutputCharset(Encoding)
    options
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
