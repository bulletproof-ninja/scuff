package scuff.web

import javax.servlet._, http._, HttpServletResponse._

private object HttpCaching {
  case class Cached(bytes: Array[Byte], lastModified: Option[Long], headers: Iterable[(String, Iterable[String])]) {
    val version = lastModified.toRight()
    val eTag = {
      val digest = java.security.MessageDigest.getInstance("MD5").digest(bytes)
      scuff.BitsBytes.hexEncode(digest).toString
    }
    def flushTo(res: HttpServletResponse) {
      headers.foreach { case (name, values) ⇒ values.foreach(res.addHeader(name, _)) }
      if (lastModified.isEmpty) res.setHeader(ETag, eTag)
      res.setContentLength(bytes.length)
      res.getOutputStream().write(bytes)
      res.flushBuffer()
    }
  }
}

private[web] sealed trait HttpCaching {
  import HttpCaching._

  private[this] val map = new scuff.LockFreeConcurrentMap[Any, Cached]

  protected def makeCacheKey(req: HttpServletRequest): Option[Any]

  private def fetchResource(res: HttpServletResponse, fetch: HttpServletResponse ⇒ Unit): Cached = {
    import collection.JavaConverters._
    val proxy = new HttpServletResponseProxy(res)
    fetch(proxy)
    new Cached(proxy.getBytes, proxy.getDateHeader(LastModified), proxy.headers)
  }
  protected[web] def respond(cacheKey: Any, req: HttpServletRequest, res: HttpServletResponse)(getResource: HttpServletResponse ⇒ Unit) {
    val cached = map.getOrElseUpdate(cacheKey, fetchResource(res, getResource))
    val expectedETag = req.getHeader(IfNoneMatch) // Can be null
    val expectedLastMod = req.getDateHeader(IfModifiedSince)
    if (cached.eTag == expectedETag || cached.lastModified.exists(_ == expectedLastMod)) {
      res.setStatus(SC_NOT_MODIFIED)
    } else {
      cached.flushTo(res)
    }
  }
}

trait HttpCachingServlet extends HttpServlet with HttpCaching {
  import HttpCaching._

  abstract override def service(req: HttpServletRequest, res: HttpServletResponse) {
    makeCacheKey(req) match {
      case Some(cacheKey) ⇒ respond(cacheKey, req, res) { res ⇒ super.service(req, res) }
      case None ⇒ super.service(req, res)
    }
  }
}

trait HttpCachingFilterMixin extends Filter with HttpCaching {
  import HttpCaching._

  abstract override def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain) = (req, res) match {
    case (req: HttpServletRequest, res: HttpServletResponse) ⇒ makeCacheKey(req) match {
      case Some(cacheKey) ⇒ respond(cacheKey, req, res) { res ⇒ super.doFilter(req, res, chain) }
      case None ⇒ super.doFilter(req, res, chain)
    }
    case _ ⇒ chain.doFilter(req, res)
  }
}
