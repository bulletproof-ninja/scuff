package scuff.web

import scala.jdk.CollectionConverters._
import javax.servlet.http._
import javax.servlet._

sealed trait PrintHeaders {
  protected def isHttpHeaderPrintingEnabled(req: HttpServletRequest): Boolean
  protected def printHeaders(req: HttpServletRequest): Unit = if (isHttpHeaderPrintingEnabled(req)) {
    def EOL = System.lineSeparator
    val buf = new java.lang.StringBuilder(200)
    val headers = req.getHeaderNames().asScala.toList
    buf append s"${req.getMethod} ${req.getRequestURL} has ${headers.size} headers:$EOL"
    for {
      header <- headers
      value <- req.getHeaders(header).asScala
    } buf append s"\t$header: $value$EOL"
    req.getServletContext().log(buf.toString)
  }
}

trait HttpHeaderPrinting extends HttpServlet with PrintHeaders {
  override def service(req: HttpServletRequest, res: HttpServletResponse): Unit = {
    printHeaders(req)
    super.service(req, res)
  }
}

abstract class HttpHeaderPrintingFilter extends Filter with PrintHeaders {
  def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain) =
    httpFilter(req, res, chain)

  @inline
  private def httpFilter(req: HttpServletRequest, res: HttpServletResponse, chain: FilterChain): Unit = {
    printHeaders(req)
    chain.doFilter(req, res)
  }
}
