package scuff.web

import javax.servlet._, http._
import java.io._
import java.net._

private object ResourceConcatenationSupport {
  final val ConcatNamesMatcher = """^\((.*)\)(\..+)?$""".r
  final val NameSplitter = """\+""".r
}

/**
  * Supports the following wildcard resource syntax:
  * * Wildcard (resources read in alphabetical order): "dir/foo*bar.js",
  *   matches any filename in dir that starts with "foo" and ends with "bar" and has the ".js" extension
  * * Listed (resources read in listed order): "dir/(foo+bar+baz+hmm).js
  */
trait ResourceConcatenationSupport extends HttpServlet {

  private def expandServletPaths(pathPrefix: String, filename: String): Seq[String] = {
    import collection.JavaConverters._
    if (filename.indexOf('*') != -1) {
      val filePattern = java.util.regex.Pattern.compile(filename.replace(".", "\\.").replace("*", ".*").replace("$", "\\$").concat("$"))
      val resourceSet = getServletContext.getResourcePaths(pathPrefix).asInstanceOf[java.util.Set[String]]
      resourceSet.asScala.filter(p ⇒ filePattern.matcher(p).find).toList.sorted
    } else {
      List(pathPrefix concat filename)
    }
  }
  private def servletPaths(req: HttpServletRequest): Seq[String] = {
    import ResourceConcatenationSupport._
    val servletPath = req.getServletPath
    val sepPos = servletPath.lastIndexOf("/") + 1
    val pathPrefix = servletPath.substring(0, sepPos)
    val filename = servletPath.substring(sepPos)
    ConcatNamesMatcher.findFirstMatchIn(filename) match {
      case None ⇒ expandServletPaths(pathPrefix, filename)
      case Some(matcher) ⇒
        val filenames = NameSplitter.split(matcher.group(1))
        val extension = Option(matcher.group(2)).getOrElse("")
        filenames.flatMap(filename ⇒ expandServletPaths(pathPrefix, filename concat extension))
    }

  }

  abstract override def doGet(req: HttpServletRequest, res: HttpServletResponse) {
    var contentLength = 0
    var lastModified = -1L
    val headers = collection.mutable.Map[String, collection.mutable.Set[String]]()
    servletPaths(req).foreach { servletPath ⇒
      val proxyReq = new HttpServletRequestWrapper(req) {
        override def getServletPath = servletPath
      }
      val proxyRes = new HttpServletResponseProxy(res) {
        override def setContentLength(len: Int) {}
      }
      super.doGet(proxyReq, proxyRes)
      proxyRes.writeTo(res.getOutputStream)
      proxyRes.getDateHeader(LastModified).foreach(lm ⇒ lastModified = math.max(lastModified, lm))
      contentLength += proxyRes.contentLength
      proxyRes.headers.withFilter(_ != LastModified).foreach {
        case (name, values) ⇒ headers.getOrElseUpdate(name, collection.mutable.Set()) ++ values
      }
    }
    headers.foreach { case (name, values) ⇒ values.foreach(value ⇒ res.addHeader(name, value)) }
    if (lastModified > -1L) res.setDateHeader(LastModified, lastModified)
    res.setContentLength(contentLength)
  }
}
