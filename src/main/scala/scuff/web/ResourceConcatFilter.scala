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
abstract class ResourceConcatFilter extends Filter {
  private final val ConcatNamesMatcher = """^\((.*)\)(\..+)?$""".r
  private final val NameSplitter = """\+""".r

  private def expandResources(ctx: ServletContext, path: String, filename: String): List[String] = {
    import collection.JavaConverters._
    if (filename.indexOf('*') != -1) {
      val filePattern = java.util.regex.Pattern.compile(filename.replace(".", "\\.").replace("*", ".*").replace("$", "\\$").concat("$"))
      val resourceSet = ctx.getResourcePaths(path).asInstanceOf[java.util.Set[String]]
      resourceSet.asScala.filter(p ⇒ filePattern.matcher(p).find).toList.sorted
    } else {
      List(path concat filename)
    }
  }
  private def extractResources(req: HttpServletRequest): List[String] = {
    val fullPath = req.servletPathInfo
    val sepPos = fullPath.lastIndexOf("/") + 1
    val pathPrefix = fullPath.substring(0, sepPos)
    val filename = fullPath.substring(sepPos)
    ConcatNamesMatcher.findFirstMatchIn(filename) match {
      case None ⇒ expandResources(req.getServletContext, pathPrefix, filename)
      case Some(matcher) ⇒
        val filenames = NameSplitter.split(matcher.group(1)).toList
        val extension = Option(matcher.group(2)).getOrElse("")
        filenames.flatMap(filename ⇒ expandResources(req.getServletContext, pathPrefix, filename concat extension))
    }
  }

  def init(config: FilterConfig) {}
  def destroy() {}

  protected def asComment(resource: String): Option[String]
  protected def printComment(comment: String, res: HttpServletResponse) = res.getOutputStream().println(comment)

  def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain) = httpFilter(req, res, chain)

  private def httpFilter(req: HttpServletRequest, res: HttpServletResponse, chain: FilterChain) {
    extractResources(req) match {
      case Nil =>
        res sendError HttpServletResponse.SC_NOT_FOUND
      case one :: Nil if one == req.servletPathInfo =>
        chain.doFilter(req, res)
      case resources => try {
        val ctx = req.getServletContext()
        Option(ctx.getMimeType(resources.head)).foreach(res.setContentType)
        val lastMod = resources.map { path ⇒
          req.getServletContext.getResource(path) match {
            case null ⇒ 0L
            case url ⇒
              val file = new java.io.File(url.toURI)
              file.lastModified
          }
        }.max
        if (req.IfModifiedSince(lastMod)) {
          res.setDateHeader(HttpHeaders.LastModified, lastMod)
          resources.foreach { resource ⇒
            val proxyReq = new HttpServletRequestWrapper(req) {
              import collection.JavaConverters._
              override def getServletPath = req.getPathInfo match {
                case null => resource
                case _ => req.getServletPath
              }
              override def getPathInfo = req.getPathInfo match {
                case null => null
                case _ => resource
              }
              private def isIfModifiedSince(name: String) = name.equalsIgnoreCase(HttpHeaders.IfModifiedSince)
              override def getDateHeader(name: String) = if (isIfModifiedSince(name)) -1 else super.getDateHeader(name)
              override def getHeader(name: String) = if (isIfModifiedSince(name)) null else super.getHeader(name)
              override def getHeaders(name: String) = if (isIfModifiedSince(name)) null else super.getHeaders(name)
              override def getHeaderNames = super.getHeaderNames().asScala.filterNot(isIfModifiedSince(_)).asJavaEnumeration
            }
            asComment(resource).foreach { comment ⇒
              printComment(comment, res)
            }
            ctx.getRequestDispatcher(resource).include(proxyReq, res)
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

}
