package scuff

package object web {
  final val LastModified = "Last-Modified"
  final val ContentLength = "Content-Length"
  final val ETag = "ETag"
  final val Age = "Age"
  final val IfNoneMatch = "If-None-Match"
  final val IfModifiedSince = "If-Modified-Since"
  def RFC822 = new java.text.SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss Z", java.util.Locale.US)
}