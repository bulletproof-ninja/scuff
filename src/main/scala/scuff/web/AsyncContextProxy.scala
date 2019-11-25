package scuff.web

import javax.servlet.AsyncContext

class AsyncContextProxy(ctx: AsyncContext)
    extends AsyncContext {

  def addListener(x$1: javax.servlet.AsyncListener,x$2: javax.servlet.ServletRequest,x$3: javax.servlet.ServletResponse): Unit =
    ctx.addListener(x$1, x$2, x$3)

  def addListener(x$1: javax.servlet.AsyncListener): Unit =
    ctx.addListener(x$1)

  def complete(): Unit =
    ctx.complete()

  def createListener[T <: javax.servlet.AsyncListener](x$1: Class[T]): T =
    ctx.createListener(x$1)

  def dispatch(x$1: javax.servlet.ServletContext,x$2: String): Unit =
    ctx.dispatch(x$1, x$2)

  def dispatch(x$1: String): Unit =
    ctx.dispatch(x$1)

  def dispatch(): Unit =
    ctx.dispatch()

  def getRequest(): javax.servlet.ServletRequest =
    ctx.getRequest()

  def getResponse(): javax.servlet.ServletResponse =
    ctx.getResponse()

  def getTimeout(): Long =
    ctx.getTimeout()

  def hasOriginalRequestAndResponse(): Boolean =
    ctx.hasOriginalRequestAndResponse()

  def setTimeout(x$1: Long): Unit =
    ctx.setTimeout(x$1)

  def start(x$1: Runnable): Unit =
    ctx.start(x$1)

}
