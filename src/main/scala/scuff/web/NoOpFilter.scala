package scuff.web

import javax.servlet._, http._

private[web] class NoOpFilter extends Filter {
  def init(config: FilterConfig) {}
  def destroy() {}
  def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain) = chain.doFilter(req, res)
}
