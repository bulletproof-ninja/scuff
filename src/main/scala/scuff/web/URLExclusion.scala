package scuff.web

import javax.servlet._
import javax.servlet.http._
import scala.util.matching._

/**
 * Apply to any filter to further discriminate
 * URL matching which cannot be done by the servlet spec.
 * <p>E.g. one can have a filter that matches
 * on all Javascript files using `"*.js"`, but
 * exclude a specific file, e.g. `"special.js"`
 * by adding that as an exclusion pattern:
 * {{{
 * val exclusionPatterns = """special\.js""".r :: Nil
 * }}}
 *
 */
trait URLExclusion extends Filter {

  /**
   * Patterns to match on URI for exclusion.
   */
  protected def exclusionPatterns: Seq[Regex]

  abstract override def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain) = httpFilter(req, res, chain)

  @inline
  private def httpFilter(req: http.HttpServletRequest, res: http.HttpServletResponse, chain: FilterChain) {
    val path = req.servletPathInfo
    if (exclusionPatterns.exists(_.pattern.matcher(path).matches)) {
      chain.doFilter(req, res)
    } else {
      super.doFilter(req, res, chain)
    }
  }
}
