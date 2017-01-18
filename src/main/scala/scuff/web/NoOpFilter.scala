package scuff.web

import javax.servlet._

private[web] abstract class NoOpFilter extends Filter {
  def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain) = chain.doFilter(req, res)
}
