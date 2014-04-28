package scuff.web

import javax.servlet._, annotation._, http._, HttpServletResponse._
import java.io._
import com.google.javascript.jscomp._
import scuff.js.ClosureCompiler

/**
 * Javascript closure compiler filter.
 */
abstract class ClosureCompilerFilter extends Filter with ClosureCompilerSupport {

  def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain) = (req, res) match {
    case (req: HttpServletRequest, res: HttpServletResponse) ⇒ httpFilter(req, res, chain)
    case _ ⇒ chain.doFilter(req, res)
  }

  private def httpFilter(req: HttpServletRequest, res: HttpServletResponse, chain: FilterChain) {
    process(req, res) { res ⇒
      chain.doFilter(req, res)
    }
  }

}

/**
 * Javascript closure compiler mixin.
 */
trait ClosureCompilation extends HttpServlet with ClosureCompilerSupport {

  abstract override def doGet(req: HttpServletRequest, res: HttpServletResponse) {
    process(req, res) { res ⇒
      super.doGet(req, res)
    }
  }

}

sealed trait ClosureCompilerSupport {

  /**
   * If an exception occurs in the Closure compiler, this
   * method is called, and the uncompressed Javascript
   * will be served instead. Rethrow the exception (or
   * throw a new one) to prevent this.
   */
  protected def onClosureError(e: Exception)

  /** Get compiler options for request. Returning `None` will not compile. */
  protected def compilerOptionsFor(req: HttpServletRequest): Option[CompilerOptions]

  protected def process(req: HttpServletRequest, res: HttpServletResponse)(useResponse: HttpServletResponse ⇒ Unit) {
    val resProxy = new HttpServletResponseProxy(res)
    useResponse(resProxy)
    compilerOptionsFor(req) match {
      case Some(options) if resProxy.getStatus == HttpServletResponse.SC_OK =>
        val uncompressed = new String(resProxy.getBytes, resProxy.getCharacterEncoding)
        try {
          val js = ClosureCompiler.compile(uncompressed, req.servletPathInfo, options)
          try {
            resProxy.resetBuffer()
            resProxy.setCharacterEncoding(ClosureCompiler.Encoding)
            resProxy.getWriter.write(js)
          } catch {
            case t: Throwable ⇒ req.getServletContext().log("Writing to response proxy failed", t)
          }
        } catch {
          case e: Exception ⇒
            onClosureError(e)
            uncompressed
        }
      case _ => // Don't compile
    }
    resProxy.propagate()
  }
}
