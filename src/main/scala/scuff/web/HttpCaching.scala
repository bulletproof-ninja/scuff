package scuff.web

import javax.servlet._
import http._
import HttpServletResponse._
import scuff.LRUHeapCache
import scala.util.control.NoStackTrace

trait HttpCaching extends HttpServlet {
  case class Cached(bytes: Array[Byte], lastModified: Option[Long], headers: List[(String, List[String])], contentType: String, encoding: String, locale: java.util.Locale) {
    require(bytes.length > 0, "Empty content of type %s".format(contentType))
    lazy val eTag = {
      val digest = java.security.MessageDigest.getInstance("MD5").digest(bytes)
      val tag = scuff.Numbers.hexEncode(digest).toString
      new ETag(tag)(false)
    }
    def flushTo(res: HttpServletResponse): Unit = {
      for ((name, values) <- headers; value <- values) res.addHeader(name, value)
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

  private lazy val defaultCache = new LRUHeapCache[Any, Cached](Int.MaxValue)
  protected def cache: scuff.Cache[Any, Cached] { type R[T] = T } = defaultCache

  /**
   *  If possible (e.g. static file system resource),
   *  return last modified of resource requested.
   *  If not possible, or not desired, an ETag hash
   *  will be used instead.
   */
  protected def fetchLastModified(req: HttpServletRequest): Option[Long]

  private def lastModified(req: HttpServletRequest): Option[Long] =
    fetchLastModified(req).map(lm => (lm / 1000) * 1000) // Shed millis

  /** Make cache key for requested resource. `None` means no caching. */
  protected def makeCacheKey(req: HttpServletRequest): Option[Any]

  private case object NotOkException extends RuntimeException with NoStackTrace

  private def fetchResource(
      req: HttpServletRequest, res: HttpServletResponse,
      buildResponse: HttpServletResponse => Unit): Cached = {
    val proxy = new HttpServletResponseProxy(res)
    buildResponse(proxy)
    if (proxy.status != SC_OK) {
      proxy.propagate()
      throw NotOkException
    } else if (proxy.getBytes.length == 0) {
      proxy.propagate(SC_NO_CONTENT)
      throw NotOkException
    }
    val lastModHeader = proxy.getDateHeaders(HttpHeaders.LastModified).headOption
    val lastMod = lastModHeader orElse lastModified(req)
    val headers = {
      val headers = proxy.headers.values.toList.map {
        case (name, values) => name -> values.toList
      }
      if (lastModHeader.isDefined) headers
      else {
        val withLastMod = lastMod
          .map(HttpHeaders.RFC_1123)
          .map(_ :: Nil)
          .map(HttpHeaders.LastModified -> _)
          .map(_ :: headers)
        withLastMod getOrElse headers
      }
    }
    new Cached(proxy.getBytes, lastMod, headers, proxy.getContentType, proxy.getCharacterEncoding, proxy.getLocale)
  }
  private def respond(cacheKey: Any, req: HttpServletRequest, res: HttpServletResponse)(buildResponse: HttpServletResponse => Unit) =
    try {
      val cached = cache.lookupOrStore(cacheKey)(fetchResource(req, res, buildResponse)) match {
        case currCache =>
          if (currCache.lastModified != lastModified(req)) { // Server cache invalid
            val freshCache = fetchResource(req, res, buildResponse)
            cache.store(cacheKey, freshCache)
            freshCache
          } else {
            currCache
          }
      }
      val isBrowserCacheInvalid = cached.lastModified match {
        case Some(lastModified) => req.IfModifiedSince(lastModified)
        case None => !req.IfNoneMatch(cached.eTag)
      }
      if (isBrowserCacheInvalid) {
        cached.flushTo(res)
      } else {
        res.setStatus(SC_NOT_MODIFIED)
      }
    } catch {
      case NotOkException => // Response already populated
    }

  override def destroy(): Unit = {
    cache.shutdown()
    super.destroy()
  }

  override def service(req: HttpServletRequest, res: HttpServletResponse): Unit = {
    if (req.getMethod == "GET") makeCacheKey(req) match {
      case Some(cacheKey) => respond(cacheKey, req, res) { res => super.service(req, res) }
      case _ => super.service(req, res)
    } else {
      super.service(req, res)
    }
  }

}
