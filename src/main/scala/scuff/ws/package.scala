package scuff

/**
  * Web service abstractions.
  */
package object ws {
  val DefaultCharset = "ISO-8859-1"
  def toReader(url: java.net.URL): java.io.BufferedReader = {
    val conn = url.openConnection()
    val mediaType = MediaType(conn.getContentType)
    val charset = mediaType.parm("charset") || DefaultCharset
    val is = conn.getInputStream()
    new java.io.BufferedReader(new java.io.InputStreamReader(is, charset))
  }
}
