package scuff.web

import javax.servlet.http.HttpServletRequest
import javax.servlet.AsyncContext

class AsyncHttpServletRequest(req: HttpServletRequest) extends HttpServletRequestProxy(req) {

  override def getContextPath() =
    if (req.isAsyncStarted) {
      req.getAttribute(AsyncContext.ASYNC_CONTEXT_PATH).asInstanceOf[String]
    } else {
      req.getContextPath()
    }
  override def getServletPath() =
    if (req.isAsyncStarted) {
      req.getAttribute(AsyncContext.ASYNC_SERVLET_PATH).asInstanceOf[String]
    } else {
      req.getServletPath()
    }
  override def getPathInfo() =
    if (req.isAsyncStarted) {
      req.getAttribute(AsyncContext.ASYNC_PATH_INFO).asInstanceOf[String]
    } else {
      req.getPathInfo()
    }
  override def getQueryString() =
    if (req.isAsyncStarted) {
      req.getAttribute(AsyncContext.ASYNC_QUERY_STRING).asInstanceOf[String]
    } else {
      req.getQueryString()
    }
  override def getRequestURI() =
    if (req.isAsyncStarted) {
      req.getAttribute(AsyncContext.ASYNC_REQUEST_URI).asInstanceOf[String]
    } else {
      req.getRequestURI()
    }
}