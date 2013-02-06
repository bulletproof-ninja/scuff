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
class ResourceConcatenationFilter extends HttpFilter {
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

  def doFilter(req: HttpServletRequest, res: HttpServletResponse, chain: FilterChain) {
    val paths = servletPaths(req)
    if (paths.size == 1) {
      chain.doFilter(req, res)
    } else try {
      val ctx = req.getServletContext()
      Option(ctx.getMimeType(paths.head)).foreach(res.setContentType)
      paths.foreach { servletPath ⇒
        val proxyReq = new HttpServletRequestWrapper(req) {
          override def getServletPath = servletPath
        }
        ctx.getRequestDispatcher(servletPath).include(proxyReq, res)
      }
      res.flushBuffer()
    } catch {
      case e: Exception ⇒
        e.printStackTrace()
        res.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR)
    }
  }

}
