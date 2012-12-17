package scuff

import java.lang.reflect.{ InvocationHandler, Proxy, Method, UndeclaredThrowableException, InvocationTargetException }

/**
 * Interface proxy factory.
 */
class Proxylicious[T](implicit manifest: ClassManifest[T]) {

  def proxy(meat: T, wrapper: Sandwich): T = {
    val handler = new InvocationHandler {
      def invoke(p: Any, method: Method, args: Array[Object]) = {
        val proxy = p.asInstanceOf[T]
        val anyArgs = args.asInstanceOf[Array[Any]]
        val include = wrapper.include(method)
        if (include) wrapper.before(proxy, method, anyArgs)
        val result: Either[Throwable, Any] = try {
          Right(method.invoke(meat, args: _*).asInstanceOf[Any])
        } catch {
          case t: InvocationTargetException ⇒ Left(t.getTargetException)
          case t: UndeclaredThrowableException => Left(t.getUndeclaredThrowable)
          case t => Left(t)
        }
        if (include) {
          wrapper.after(proxy, method, anyArgs, result).asInstanceOf[Object]
        } else result match {
          case Left(t) ⇒ throw t
          case Right(r: Object) ⇒ r
        }
      }
    }
    Proxy.newProxyInstance(manifest.erasure.getClassLoader, Array(manifest.erasure), handler).asInstanceOf[T]
  }
  trait Sandwich {
    /**
     * Filter. Determines if `before` and `after` is called. 
     */
    def include(method: Method): Boolean
    /**
     * Called before invoking method.
     */
    def before(proxy: T, method: Method, args: Array[Any])
    /**
     * Called after invoking method.
     */
    def after(proxy: T, method: Method, args: Array[Any], result: Either[Throwable, Any]): Any
  }

}

