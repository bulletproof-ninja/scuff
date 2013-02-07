package scuff.web

import javax.servlet._

trait URLExclusion extends HttpFilter {
  def exclusionPatterns: Seq[util.matching.Regex]
  abstract override def doFilter(req: http.HttpServletRequest, res: http.HttpServletResponse, chain: FilterChain) {
    val path = req.getServletPath
    if (exclusionPatterns.exists(_.pattern.matcher(path).matches)) {
      chain.doFilter(req, res)
    } else {
      super.doFilter(req, res, chain)
    }
  }
}
