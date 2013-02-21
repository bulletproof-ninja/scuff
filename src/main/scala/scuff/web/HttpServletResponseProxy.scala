package scuff.web

import java.io._
import javax.servlet._, http._
import collection.mutable._
import collection.JavaConverters._

class HttpServletResponseProxy(delegate: HttpServletResponse) extends HttpServletResponseWrapper(delegate) {
  import scala.language.reflectiveCalls
  val headers = Map[String, Buffer[String]]()
  private val out = new ServletOutputStream {
    private val _buffer = new ByteArrayOutputStream(4096 * 2)
    def buffer = {
      flushBuffer()
      _buffer
    }
    def write(byte: Int) = _buffer.write(byte)
  }
  private var writer: Option[PrintWriter] = None
  override def getOutputStream: ServletOutputStream = out
  override def getWriter = writer.getOrElse {
    val w = new PrintWriter(new OutputStreamWriter(out, getCharacterEncoding))
    writer = Some(w)
    w
  }
  override def isCommitted() = false
  override def flushBuffer = writer.foreach(_.flush())
  override def addHeader(name: String, value: String) = headers.getOrElseUpdate(name, Buffer[String]()) += value
  override def addIntHeader(name: String, value: Int) = addHeader(name, value.toString)
  override def addDateHeader(name: String, value: Long) = addHeader(name, dateFmt.format(new java.util.Date(value)))

  override def setHeader(name: String, value: String) = headers.put(name, Buffer(value))
  override def setIntHeader(name: String, value: Int) = setHeader(name, value.toString)
  override def setDateHeader(name: String, value: Long) = setHeader(name, dateFmt.format(new java.util.Date(value)))

  override def getHeader(name: String): String = headers.getOrElse(name, Buffer.empty).headOption.getOrElse(null)
  override def getHeaders(name: String) = headers.getOrElse(name, Buffer.empty).asJavaCollection
  override def containsHeader(name: String) = headers.contains(name)
  override def getHeaderNames() = headers.keySet.asJavaCollection

  def getBytes = out.buffer.toByteArray()
  def contentLength = out.buffer.size
  def writeTo(dest: OutputStream) = out.buffer.writeTo(dest)
  private[this] val dateFmt = RFC822
  def getDateHeader(name: String): Option[Long] = try { Option(getHeader(name)).map(dateFmt.parse(_).getTime) } catch { case _: Exception ⇒ None }
}
