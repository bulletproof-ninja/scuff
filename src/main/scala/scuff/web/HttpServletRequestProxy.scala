package scuff.web

import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletRequestWrapper

class HttpServletRequestProxy(req: HttpServletRequest) extends HttpServletRequestWrapper(req) {
  override def toString() = req.toString()
}
