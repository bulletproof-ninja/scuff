package scuff

import java.io.{ BufferedReader, InputStream, InputStreamReader, Reader, StringReader }
import java.nio.charset.Charset

import javax.script.ScriptEngineManager

import scala.language.implicitConversions

package object js {

  def UTF8 = Charset.forName("UTF-8")

  def toJavascript(options: Seq[(Symbol, Any)]): String = {
    val sb = new java.lang.StringBuilder
      def appendValue(value: Any) {
        value match {
          case nb @ (_: java.lang.Number | _: Boolean | null) => sb append nb
          case s: Seq[_] =>
            sb append "["
            s.foreach(appendValue)
            sb append "]"
          case s => sb append '"' append s append '"'
        }
        sb append ','
      }
    sb append "{"
    options.foreach {
      case (prop, value) =>
        sb append prop.name append ':'
        appendValue(value)
    }
    if (options.isEmpty) {
      sb append "}"
    } else {
      sb.replace(sb.length - 1, sb.length, "}")
    }
    sb.toString
  }

  implicit def stringToReader(string: String) = new StringReader(string)

  implicit def readerToString(reader: Reader) = {
    val sb = new java.lang.StringBuilder(1024)
    val bufReader = reader match {
      case br: BufferedReader => br
      case _ => new BufferedReader(reader)
    }
    var line = bufReader.readLine()
    while (line != null) {
      sb append line append '\n'
      line = bufReader.readLine()
    }
    sb.toString
  }

  implicit def streamToReader(is: InputStream) = new InputStreamReader(is, UTF8)
  implicit def streamToString(is: InputStream) = readerToString(streamToReader(is))

  private lazy val scriptEngineMgr = new ScriptEngineManager
  private[js] def newJavascriptEngine(): javax.script.ScriptEngine = newJavascriptEngine("javascript")
  private[js] def newJavascriptEngine(name: String): javax.script.ScriptEngine = scriptEngineMgr.getEngineByName(name)

}
