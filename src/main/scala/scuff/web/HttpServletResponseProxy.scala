package scuff.web

import java.io._
import javax.servlet._, http._
import collection._
import JavaConverters._
import beans.BeanProperty
import HttpServletResponse._
import java.util.Locale
import language.implicitConversions

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
    case Some(w) ⇒ w.flush()
    case None ⇒ out match {
      case None ⇒ // Ignore
      case Some(out) ⇒ out.flush()
    }
  }

  def addHeader(name: String, value: String) = headers.getOrElseUpdate(name.toLowerCase, (name -> mutable.Buffer[String]()))._2 += value
  def addIntHeader(name: String, value: Int) = addHeader(name, value.toString)
  def addDateHeader(name: String, value: Long) = addHeader(name, dateFmt.format(new java.util.Date(value)))

  def setHeader(name: String, value: String) = headers.put(name.toLowerCase, (name -> mutable.Buffer(value)))
  def setIntHeader(name: String, value: Int) = setHeader(name, value.toString)
  def setDateHeader(name: String, value: Long) = setHeader(name, dateFmt.format(new java.util.Date(value)))

  def getHeader(name: String): String = headers.get(name.toLowerCase) match {
    case None ⇒ null
    case Some(header) ⇒ header._2.headOption.getOrElse(null)
  }
  def getHeaders(name: String) = headers.get(name.toLowerCase) match {
    case None ⇒ java.util.Collections.emptyList()
    case Some(headers) ⇒ headers._2.asJavaCollection
  }
  def containsHeader(name: String) = headers.contains(name.toLowerCase)
  def getHeaderNames() = headers.values.map(_._1).asJavaCollection

  def getBytes: Array[Byte] = out match {
    case None ⇒ Array()
    case Some(out) ⇒ out.buffer.toByteArray()
  }
  def getBufferSize = out match {
    case None ⇒ 0
    case Some(out) ⇒ out.buffer.size
  }
  private[this] var bufferSizeHint = 4096 * 2
  def setBufferSize(hint: Int) = bufferSizeHint = hint
  private[this] val dateFmt = RFC822
  def getDateHeaders(name: String): Seq[Long] = try {
    headers.get(name.toLowerCase) match {
      case None ⇒ Seq.empty
      case Some((_, values)) ⇒ values.map(dateFmt.parse(_).getTime)
    }
  }

  var status = delegate.getStatus()
  var message: Option[String] = None

  def getStatus = status
  def setStatus(code: Int) = status = code
  def setStatus(code: Int, message: String) {
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

  def sendError(code: Int) {
    status = code
    inError = true
  }

  def sendError(code: Int, message: String) {
    status = code
    this.message = Option(message)
    inError = true
  }
  var redirect: Option[String] = None
  def sendRedirect(redir: String) = redirect = Option(redir)

  @reflect.BeanProperty
  var locale: Locale = delegate.getLocale()

  @reflect.BeanProperty
  var characterEncoding = delegate.getCharacterEncoding()

  @reflect.BeanProperty
  var contentType = delegate.getContentType()

  private var contentLength: Option[Int] = None
  def setContentLength(len: Int) = Some(len)

  def reset() {
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
  def resetBuffer() {
    writer = None
    out = None
  }

  /**
   * Propagate from proxy to delegate.
   */
  def propagate(stat: Int = status) {
    for (cookie ← cookies) delegate.addCookie(cookie)
    if (inError) message match {
      case None ⇒ delegate.sendError(status)
      case Some(message) ⇒ delegate.sendError(status, message)
    }
    else redirect match {
      case Some(redirect) ⇒ delegate.sendRedirect(redirect)
      case None ⇒
        for ((name, values) ← headers.values; value ← values) {
          delegate.addHeader(name, value)
        }
        message match {
          case None ⇒ delegate.setStatus(stat)
          case Some(message) ⇒ delegate.setStatus(stat, message)
        }
        delegate.setCharacterEncoding(characterEncoding)
        contentLength.foreach(delegate.setContentLength)
        delegate.setContentType(contentType)
        delegate.setLocale(locale)
        out.foreach(_.buffer.writeTo(delegate.getOutputStream))
    }
  }
}
