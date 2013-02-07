package scuff.web

import javax.servlet._, http._

trait HttpFilter extends Filter {
  final def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain): Unit = (req, res) match {
    case (req: HttpServletRequest, res: HttpServletResponse) ⇒ doFilter(req, res, chain)
    case _ ⇒ chain.doFilter(req, res)
  }

  protected def doFilter(req: HttpServletRequest, res: HttpServletResponse, chain: FilterChain): Unit

}

private[web] class NoOpFilter extends Filter {
  def init(config: FilterConfig) {}
  def destroy() {}
  def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain) = chain.doFilter(req, res)
}

private[web] class NoOpHttpFilter extends HttpFilter {
  def init(config: FilterConfig) {}
  def destroy() {}
  def doFilter(req: HttpServletRequest, res: HttpServletResponse, chain: FilterChain) = chain.doFilter(req, res)
}
