package scuff.web

import javax.servlet._, annotation._, http._, HttpServletResponse._
import java.io._
import com.google.javascript.jscomp._
import scuff.js.ClosureCompiler

/**
 * Javascript closure compiler filter.
 */
abstract class ClosureCompilerFilter extends Filter {

  /**
    * If an exception occurs in the Closure compiler, this
    * method is called, and the uncompressed Javascript
    * will be served instead. Rethrow the exception (or
    * throw a new one) to prevent this.
    */
  protected def onCompileError(e: Exception)

  def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain) = (req, res) match {
    case (req: HttpServletRequest, res: HttpServletResponse) ⇒ httpFilter(req, res, chain)
    case _ ⇒ chain.doFilter(req, res)
  }

  private def httpFilter(req: HttpServletRequest, res: HttpServletResponse, chain: FilterChain) {
    val resProxy = new HttpServletResponseProxy(res)
    chain.doFilter(req, resProxy)
    val uncompressed = new String(resProxy.getBytes, resProxy.getCharacterEncoding)
    try {
      val js = ClosureCompiler.compile(uncompressed, req.getServletPath)
      try {
        resProxy.resetBuffer()
        resProxy.setCharacterEncoding(ClosureCompiler.Encoding)
        resProxy.getWriter.write(js)
      } catch {
        case e: Exception ⇒
          e.printStackTrace()
      }
    } catch {
      case e: Exception ⇒
        onCompileError(e)
        uncompressed
    }
    resProxy.propagate()
  }

}

