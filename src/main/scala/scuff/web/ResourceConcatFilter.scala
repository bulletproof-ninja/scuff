package scuff.web

import javax.servlet._, http._

/**
 * Supports the following wildcard resource syntax:
 * * Wildcard (resources read in alphabetical order): "dir/foo*bar.js",
 *   matches any filename in dir that starts with "foo" and ends with "bar" and has the ".js" extension
 * * Listed (resources read in listed order): "dir/(foo+bar+baz+hmm).js
 */
abstract class ResourceConcatFilter extends Filter {
  private final val DefaultConcatGroupMatcher = """^\((.*)\)(\..+)?$""".r
  private final val DefaultNameSplitter = """[\+\|\,\&]""".r

  /**
   * Must match and capture two groups:
   * 1) The resource names, which will be split by `NameSplitter`
   * 2) The common file extension
   */
  protected def ConcatGroupMatcher = DefaultConcatGroupMatcher
  /**
   * Must split the names captured by group 1 of `ConcatGroupMatcher`.
   */
  protected def NameSplitter = DefaultNameSplitter

  private def expandResources(ctx: ServletContext, path: String, filename: String): List[String] = {
    import collection.JavaConverters._
    if (filename.indexOf('*') != -1) {
      val filePattern = java.util.regex.Pattern.compile(filename.replace(".", "\\.").replace("*", ".*").replace("$", "\\$").concat("$"))
      val resourceSet = ctx.getResourcePaths(path).asInstanceOf[java.util.Set[String]]
      resourceSet.asScala.filter(p => filePattern.matcher(p).find).toList.sorted
    } else {
      List(path concat filename)
    }
  }
  private def extractResources(req: HttpServletRequest): List[String] = {
    val fullPath = req.servletPathInfo
    val sepPos = fullPath.lastIndexOf("/") + 1
    val pathPrefix = fullPath.substring(0, sepPos)
    val filename = fullPath.substring(sepPos)
    ConcatGroupMatcher.findFirstMatchIn(filename) match {
      case None => expandResources(req.getServletContext, pathPrefix, filename)
      case Some(matcher) =>
        val filenames = NameSplitter.split(matcher.group(1)).toList
        val extension = Option(matcher.group(2)).getOrElse("")
        filenames.flatMap(filename => expandResources(req.getServletContext, pathPrefix, filename concat extension))
    }
  }

  /** Include resource name as comment. */
  protected def asComment(resource: String): Option[String]
  /** Print comment. */
  protected def printComment(comment: String, res: HttpServletResponse) = res.getOutputStream().println(comment)

  /**
   * Max age, in seconds, for concatenated resources.
   * @param req The HTTP request.
   * Passed for querying, in case max-age depends on the request.
   */
  protected def maxAge(req: HttpServletRequest): Int

  def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain) = httpFilter(req, res, chain)

  @inline
  private def httpFilter(req: HttpServletRequest, res: HttpServletResponse, chain: FilterChain): Unit = {
    extractResources(req) match {
      case Nil =>
        res sendError HttpServletResponse.SC_NOT_FOUND
      case one :: Nil if one == req.servletPathInfo =>
        chain.doFilter(req, res)
      case resources => try {
        val ctx = req.getServletContext()
        Option(ctx.getMimeType(resources.head)).foreach(res.setContentType)
        val lastMod = resources.map { path =>
          req.getServletContext.getResource(path) match {
            case null => 0L
            case url =>
              val file = new java.io.File(url.toURI)
              file.lastModified
          }
        }.max
        if (req.IfModifiedSince(lastMod)) {
          res.setLastModified(lastMod).setMaxAge(maxAge(req))
          resources.foreach { resource =>
            val proxyReq = new HttpServletRequestProxy(req) {
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
              override def getHeaderNames = super.getHeaderNames().asScala.filterNot(h => isIfModifiedSince(h)).asJavaEnumeration
            }
            asComment(resource).foreach { comment =>
              printComment(comment, res)
            }
            var resStatus = 0
            var resMsg: Option[String] = None
            val proxyRes = new HttpServletResponseWrapper(res) {
              override def sendError(sc: Int): Unit = {
                super.sendError(sc)
                resStatus = sc
              }
              override def sendError(sc: Int, msg: String): Unit = {
                super.sendError(sc, msg)
                resStatus = sc
                resMsg = Option(msg)
              }
              override def setStatus(sc: Int): Unit = {
                super.setStatus(sc)
                resStatus = sc
              }
              override def setStatus(sc: Int, msg: String): Unit = {
                super.setStatus(sc, msg)
                resStatus = sc
                resMsg = Option(msg)
              }
            }
            ctx.getRequestDispatcher(resource).include(proxyReq, proxyRes)
            checkDispatchStatus(resource, resStatus, resMsg)
          }
          res.flushBuffer()
        } else {
          res.setStatus(HttpServletResponse.SC_NOT_MODIFIED)
        }
      } catch {
        case t: Throwable =>
          req.getServletContext.log(getClass.getName, t)
          res.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR)
      }
    }
  }

  protected def checkDispatchStatus(resource: String, sc: Int, msg: Option[String]): Unit = {
    if (sc >= 400) {
      val withMsg = msg.map(": " + _).getOrElse("")
      throw new IllegalStateException(s"Failed to dispatch to $resource$withMsg")
    }
  }

}
