package scuff.web

import java.io.{ ByteArrayOutputStream, OutputStreamWriter, PrintWriter }
import java.util.Locale

import scala.collection.{ Seq, mutable }
import scala.jdk.CollectionConverters._
import scala.util.Try

import javax.servlet.ServletOutputStream
import javax.servlet.http.{ Cookie, HttpServletResponse }
import javax.servlet.http.HttpServletResponse.SC_OK

class HttpServletResponseProxy(delegate: HttpServletResponse) extends HttpServletResponse {

  val headers = mutable.Map[String, (String, mutable.Buffer[String])]()

  private class Output extends ServletOutputStream {
    private val _buffer = new ByteArrayOutputStream(bufferSizeHint)
    def buffer = {
      flushBuffer()
      _buffer
    }
    def write(byte: Int) = _buffer.write(byte)
  }

  private[this] var out: Option[Output] = None
  private def forceOut = out.getOrElse {
    val o = new Output
    out = Some(o)
    o
  }
  private var writer: Option[PrintWriter] = None
  def getOutputStream: ServletOutputStream = forceOut
  def getWriter = writer.getOrElse {
    val w = new PrintWriter(new OutputStreamWriter(forceOut, getCharacterEncoding))
    writer = Some(w)
    w
  }
  def isCommitted() = false
  def flushBuffer() = writer match {
    case Some(w) => w.flush()
    case None => out match {
      case None => // Ignore
      case Some(out) => out.flush()
    }
  }

  def addHeader(name: String, value: String) = headers.getOrElseUpdate(name.toLowerCase, (name -> mutable.Buffer[String]()))._2 += value
  def addIntHeader(name: String, value: Int) = addHeader(name, value.toString)
  def addDateHeader(name: String, value: Long) = addHeader(name, dateFmt(value))

  def setHeader(name: String, value: String) = headers.put(name.toLowerCase, (name -> mutable.Buffer(value)))
  def setIntHeader(name: String, value: Int) = setHeader(name, value.toString)
  def setDateHeader(name: String, value: Long) = setHeader(name, dateFmt(value))

  def getHeader(name: String): String = headers.get(name.toLowerCase) match {
    case None => null
    case Some(header) => header._2.headOption.getOrElse(null)
  }
  def getHeaders(name: String) = headers.get(name.toLowerCase) match {
    case None => java.util.Collections.emptyList()
    case Some(headers) => headers._2.asJavaCollection
  }
  def containsHeader(name: String) = headers.contains(name.toLowerCase)
  def getHeaderNames() = headers.values.map(_._1).asJavaCollection

  def getBytes: Array[Byte] = out match {
    case None => Array()
    case Some(out) => out.buffer.toByteArray()
  }
  def getBufferSize = out match {
    case None => 0
    case Some(out) => out.buffer.size
  }
  private[this] var bufferSizeHint = 4096 * 2
  def setBufferSize(hint: Int) = bufferSizeHint = hint
  private def dateFmt(date: Long): String = HttpHeaders.RFC_1123(date)
  private def parseDate(str: String): Long = HttpHeaders.RFC_1123(str).toEpochSecond * 1000
  def getDateHeaders(name: String): Seq[Long] =
    headers.get(name.toLowerCase) match {
      case None => Seq.empty
      case Some((_, values)) => values.flatMap(t => Try(parseDate(t)).toOption)
    }

  var status = delegate.getStatus()
  var message: Option[String] = None

  def getStatus = status
  def setStatus(code: Int) = status = code
  def setStatus(code: Int, message: String): Unit = {
    status = code
    this.message = Option(message)
  }

  var cookies: List[Cookie] = Nil
  def addCookie(cookie: Cookie) = cookies = cookie :: cookies

  def encodeRedirectUrl(url: String) = delegate.encodeRedirectURL(url)
  def encodeRedirectURL(url: String) = delegate.encodeRedirectURL(url)
  def encodeUrl(url: String) = delegate.encodeURL(url)
  def encodeURL(url: String) = delegate.encodeURL(url)

  var inError = false

  def sendError(code: Int): Unit = {
    status = code
    inError = true
  }

  def sendError(code: Int, message: String): Unit = {
    status = code
    this.message = Option(message)
    inError = true
  }
  var redirect: Option[String] = None
  def sendRedirect(redir: String) = redirect = Option(redir)

  @beans.BeanProperty
  var locale: Locale = delegate.getLocale()

  @beans.BeanProperty
  var characterEncoding = delegate.getCharacterEncoding()

  @beans.BeanProperty
  var contentType = delegate.getContentType()

  private var contentLength: Option[Int] = None
  def setContentLength(len: Int) = this.contentLength = Some(len)

  def reset(): Unit = {
    headers.clear()
    cookies = Nil
    inError = false
    message = None
    contentType = delegate.getContentType()
    characterEncoding = delegate.getCharacterEncoding()
    locale = delegate.getLocale()
    status = SC_OK
    contentLength = None
    resetBuffer()
  }
  def resetBuffer(): Unit = {
    writer = None
    out = None
  }

  /**
   * Propagate from proxy to delegate.
   */
  def propagate(stat: Int = status): Unit = {
    for (cookie <- cookies) delegate.addCookie(cookie)
    if (inError) message match {
      case None => delegate.sendError(status)
      case Some(message) => delegate.sendError(status, message)
    }
    else redirect match {
      case Some(redirect) => delegate.sendRedirect(redirect)
      case None =>
        for ((name, values) <- headers.values; value <- values) {
          delegate.addHeader(name, value)
        }
        message match {
          case None => delegate.setStatus(stat)
          case Some(message) => delegate.setStatus(stat, message)
        }
        delegate.setCharacterEncoding(characterEncoding)
        contentLength.foreach(delegate.setContentLength)
        delegate.setContentType(contentType)
        delegate.setLocale(locale)
        out.foreach(_.buffer.writeTo(delegate.getOutputStream))
    }
  }
}
