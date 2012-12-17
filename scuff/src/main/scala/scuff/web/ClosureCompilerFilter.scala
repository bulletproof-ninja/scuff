package scuff.web

import javax.servlet._, annotation._, http._, HttpServletResponse._
import java.io._
import com.google.javascript.jscomp._
import scuff.js.ClosureCompiler

/**
 * Javascript closure compiler filter.
 */
abstract class ClosureCompilerFilter extends HttpFilter {

  /**
    * If an exception occurs in the Closure compiler, this
    * method is called, and the uncompressed Javascript
    * will be served instead. Rethrow the exception (or
    * throw a new one) to prevent this.
    */
  protected def onCompileError(e: Exception)

  def doFilter(req: HttpServletRequest, res: HttpServletResponse, chain: FilterChain) {
    val resProxy = new HttpServletResponseProxy(res)
    chain.doFilter(req, resProxy)
    val uncompressed = new String(resProxy.getBytes, resProxy.getCharacterEncoding)
    val js = try {
      ClosureCompiler.compile(uncompressed, req.getServletPath)
    } catch {
      case e: Exception ⇒
        onCompileError(e)
        uncompressed
    }
    res.setContentType(resProxy.getContentType)
    res.setCharacterEncoding(ClosureCompiler.Encoding)
    val writer = res.getWriter
    writer.write(js)
    writer.flush()
  }

}

