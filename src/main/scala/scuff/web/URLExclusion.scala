package scuff.web

import javax.servlet._
import javax.servlet.http._

trait URLExclusion extends Filter {
  def exclusionPatterns: Seq[util.matching.Regex]

  abstract override def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain) = (req, res) match {
    case (req: HttpServletRequest, res: HttpServletResponse) ⇒ httpFilter(req, res, chain)
    case _ ⇒ super.doFilter(req, res, chain)
  }

  private def httpFilter(req: http.HttpServletRequest, res: http.HttpServletResponse, chain: FilterChain) {
    val path = req.getServletPath
    if (exclusionPatterns.exists(_.pattern.matcher(path).matches)) {
      chain.doFilter(req, res)
    } else {
      super.doFilter(req, res, chain)
    }
  }
}
