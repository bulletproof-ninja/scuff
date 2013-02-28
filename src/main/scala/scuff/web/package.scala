package scuff

package object web {
  final val LastModified = "Last-Modified"
  final val ContentLength = "Content-Length"
  final val ETag = "ETag"
  final val Age = "Age"
  final val IfNoneMatch = "If-None-Match"
  final val IfModifiedSince = "If-Modified-Since"
  private val RFC822Pool = new ThreadLocal[java.text.SimpleDateFormat] {
    override def initialValue = new java.text.SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss Z", java.util.Locale.US)
  }
  def RFC822 = RFC822Pool.get()
}