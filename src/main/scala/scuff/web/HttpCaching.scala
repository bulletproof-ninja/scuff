package scuff.web

import javax.servlet._
import http._
import HttpServletResponse._
import scuff.LRUHeapCache

object HttpCaching {
  case class Cached(bytes: Array[Byte], lastModified: Option[Long], headers: Iterable[(String, Iterable[String])], contentType: String, encoding: String, locale: java.util.Locale) {
    require(bytes.length > 0, "Empty content of type %s".format(contentType))
    lazy val eTag = {
      val digest = java.security.MessageDigest.getInstance("MD5").digest(bytes)
      val tag = scuff.Numbers.hexEncode(digest).toString
      new ETag(tag)(false)
    }
    def flushTo(res: HttpServletResponse) {
      for ((name, values) ← headers; value ← values) res.addHeader(name, value)
      if (lastModified.isEmpty) {
        eTag.addTo(res)
      }
      res.setContentType(contentType)
      res.setCharacterEncoding(encoding)
      res.setLocale(locale)
      res.setContentLength(bytes.length)
      res.getOutputStream().write(bytes)
      res.flushBuffer()
    }
  }
}

trait HttpCaching extends HttpServlet {
  import HttpCaching._

  private lazy val defaultCache = new LRUHeapCache[Any, Cached](Int.MaxValue)
  protected def cache: scuff.Cache[Any, Cached] = defaultCache

  protected def fetchLastModified(req: HttpServletRequest): Option[Long]
  protected def makeCacheKey(req: HttpServletRequest): Option[Any]

  private case object NotOkException extends RuntimeException {
    override def fillInStackTrace: Throwable = this
  }

  private def fetchResource(res: HttpServletResponse, fetch: HttpServletResponse ⇒ Unit): Cached = {
    import collection.JavaConverters._
    val proxy = new HttpServletResponseProxy(res)
    fetch(proxy)
    if (proxy.status != SC_OK) {
      proxy.propagate()
      throw NotOkException
    } else if (proxy.getBytes.length == 0) {
      proxy.propagate(SC_NO_CONTENT)
      throw NotOkException
    }
    val lastMod = proxy.getDateHeaders(HttpHeaders.LastModified).headOption
    new Cached(proxy.getBytes, lastMod, proxy.headers.values, proxy.getContentType, proxy.getCharacterEncoding, proxy.getLocale)
  }
  private def respond(cacheKey: Any, req: HttpServletRequest, res: HttpServletResponse)(getResource: HttpServletResponse ⇒ Unit) =
    try {
      val cached = cache.lookupOrStore(cacheKey)(fetchResource(res, getResource)) match {
        case currCache ⇒
          if (currCache.lastModified != fetchLastModified(req)) { // Server cache invalid
            val freshCache = fetchResource(res, getResource)
            cache.store(cacheKey, freshCache)
            freshCache
          } else {
            currCache
          }
      }
      val isClientCacheInvalid = cached.lastModified match {
        case Some(lastModified) ⇒ req.IfModifiedSince(lastModified)
        case None ⇒ !req.IfNoneMatch(cached.eTag)
      }
      if (isClientCacheInvalid) {
        cached.flushTo(res)
      } else {
        res.setStatus(SC_NOT_MODIFIED)
      }
    } catch {
      case NotOkException ⇒ // Response already populated
    }
  abstract override def destroy {
    cache.shutdown()
    super.destroy()
  }

  abstract override def doGet(req: HttpServletRequest, res: HttpServletResponse) {
    makeCacheKey(req) match {
      case Some(cacheKey) ⇒ respond(cacheKey, req, res) { res ⇒ super.doGet(req, res) }
      case _ ⇒ super.doGet(req, res)
    }
  }

}
