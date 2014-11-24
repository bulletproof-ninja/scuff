package scuff.web

import javax.servlet._
import http._

sealed trait RobotsDirective {
  protected def robotDirective: String
  @inline
  protected final def addHeader(res: HttpServletResponse) {
    res.addHeader("X-Robots-Tag", robotDirective)
  }
}

trait RobotsDirectiveServlet extends HttpServlet with RobotsDirective {
  abstract override def service(req: HttpServletRequest, res: HttpServletResponse) {
    addHeader(res)
    super.service(req, res)
  }
}

/** Extend any servlet with this trait to prevent search engine indexing. */
trait RobotsNoIndex extends RobotsDirectiveServlet {
  protected def robotDirective: String = "noindex"
}

abstract class RobotsDirectiveFilter extends Filter with RobotsDirective {

  protected final def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain) = httpFilter(req, res, chain)

  @inline
  private def httpFilter(req: HttpServletRequest, res: HttpServletResponse, chain: FilterChain) {
    addHeader(res)
    chain.doFilter(req, res)
  }

}
