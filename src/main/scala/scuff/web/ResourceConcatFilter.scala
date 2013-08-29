package scuff.web

import javax.servlet._, http._
import java.io._
import java.net._

/**
 * Supports the following wildcard resource syntax:
 * * Wildcard (resources read in alphabetical order): "dir/foo*bar.js",
 *   matches any filename in dir that starts with "foo" and ends with "bar" and has the ".js" extension
 * * Listed (resources read in listed order): "dir/(foo+bar+baz+hmm).js
 */
class ResourceConcatFilter extends Filter {
  private final val ConcatNamesMatcher = """^\((.*)\)(\..+)?$""".r
  private final val NameSplitter = """\+""".r

  private def expandServletPaths(ctx: ServletContext, pathPrefix: String, filename: String): Seq[String] = {
    import collection.JavaConverters._
    if (filename.indexOf('*') != -1) {
      val filePattern = java.util.regex.Pattern.compile(filename.replace(".", "\\.").replace("*", ".*").replace("$", "\\$").concat("$"))
      val resourceSet = ctx.getResourcePaths(pathPrefix).asInstanceOf[java.util.Set[String]]
      resourceSet.asScala.filter(p ⇒ filePattern.matcher(p).find).toList.sorted
    } else {
      List(pathPrefix concat filename)
    }
  }
  private def servletPaths(req: HttpServletRequest): Seq[String] = {
    val servletPath = req.getServletPath
    val sepPos = servletPath.lastIndexOf("/") + 1
    val pathPrefix = servletPath.substring(0, sepPos)
    val filename = servletPath.substring(sepPos)
    ConcatNamesMatcher.findFirstMatchIn(filename) match {
      case None ⇒ expandServletPaths(req.getServletContext, pathPrefix, filename)
      case Some(matcher) ⇒
        val filenames = NameSplitter.split(matcher.group(1))
        val extension = Option(matcher.group(2)).getOrElse("")
        filenames.flatMap(filename ⇒ expandServletPaths(req.getServletContext, pathPrefix, filename concat extension))
    }
  }

  def init(config: FilterConfig) {}
  def destroy() {}

  def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain) = (req, res) match {
    case (req: HttpServletRequest, res: HttpServletResponse) ⇒ httpFilter(req, res, chain)
    case _ ⇒ chain.doFilter(req, res)
  }

  private def httpFilter(req: HttpServletRequest, res: HttpServletResponse, chain: FilterChain) {
    val paths = servletPaths(req)
    if (paths.size == 1) {
      chain.doFilter(req, res)
    } else try {
      val ctx = req.getServletContext()
      Option(ctx.getMimeType(paths.head)).foreach(res.setContentType)
      val lastMod = paths.map { path ⇒
        req.getServletContext.getResource(path) match {
          case null ⇒ 0L
          case url ⇒
            val file = new java.io.File(url.toURI)
            file.lastModified
        }
      }.max
      if (req.IfModifiedSince(lastMod)) {
        res.setDateHeader(HttpHeaders.LastModified, lastMod)
        paths.foreach { servletPath ⇒
          val proxyReq = new HttpServletRequestWrapper(req) {
            import collection.JavaConverters._
            override def getServletPath = servletPath
            private def isIfModifiedSince(name: String) = name.equalsIgnoreCase(HttpHeaders.IfModifiedSince)
            override def getDateHeader(name: String) = if (isIfModifiedSince(name)) -1 else super.getDateHeader(name)
            override def getHeader(name: String) = if (isIfModifiedSince(name)) null else super.getHeader(name)
            override def getHeaders(name: String) = if (isIfModifiedSince(name)) null else super.getHeaders(name)
            override def getHeaderNames = super.getHeaderNames().asScala.filterNot(isIfModifiedSince(_)).asJavaEnumeration
          }
          ctx.getRequestDispatcher(servletPath).include(proxyReq, res)
        }
        res.flushBuffer()
      } else {
        res.setStatus(HttpServletResponse.SC_NOT_MODIFIED)
      }
    } catch {
      case t: Throwable ⇒
        req.getServletContext.log(getClass.getName, t)
        res.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR)
    }
  }

}
