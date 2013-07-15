package scuff

/**
  * Web service abstractions.
  */
package object ws {
  val DefaultCharset = "ISO-8859-1"
  def toReader(url: java.net.URL): java.io.BufferedReader = {
    val conn = url.openConnection()
    val mimeType = new javax.activation.MimeType(conn.getContentType)
    val charset = Option(mimeType.getParameter("charset")).getOrElse(DefaultCharset)
    val is = conn.getInputStream()
    new java.io.BufferedReader(new java.io.InputStreamReader(is, charset))
  }
}
