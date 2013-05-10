package scuff

import org.mozilla.javascript._
import java.io._
import java.nio.charset.Charset

package object js {
  
  def UTF8 = Charset.forName("UTF-8")
  
  def toJavascript(options: Seq[(Symbol, Any)]): String = {
    val sb = new java.lang.StringBuilder
      def appendValue(value: Any) {
        value match {
          case nb @ (_: java.lang.Number | _: Boolean | null) ⇒ sb append nb
          case s: Seq[_] ⇒
            sb append "[" 
            s.foreach(appendValue)
            sb append "]"
          case s ⇒ sb append '"' append s append '"'
        }
        sb append ','
      }
    sb append "{"
    options.foreach {
      case (prop, value) ⇒
        sb append prop.name append ':'
        appendValue(value)
    }
    sb append "}"
    sb.toString
  }

  def withContext[T](block: (Context) ⇒ T): T = {
    val context = Context.enter()
    try {
      context.setOptimizationLevel(-1) // Prevent 64K bytecode limit
      block(context)
    } finally {
      Context.exit()
    }
  }
  
  implicit def stringToReader(string: String) = new StringReader(string)
  
  implicit def readerToString(reader: Reader) = {
    val sb = new java.lang.StringBuilder(1024)
    val bufReader = if (reader.isInstanceOf[BufferedReader]) reader.asInstanceOf[BufferedReader] else new BufferedReader(reader)
    var line = bufReader.readLine()
    while (line != null) {
      sb append line append '\n'
      line = bufReader.readLine()
    }
    sb.toString
  }

  implicit def streamToReader(is: InputStream) = new InputStreamReader(is, UTF8)
  implicit def streamToString(is: InputStream) = readerToString(streamToReader(is))

}