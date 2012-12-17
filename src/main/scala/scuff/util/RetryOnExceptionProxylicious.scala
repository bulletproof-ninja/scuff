package scuff.util

import scuff.Proxylicious
import java.lang.reflect.Method

/**
 * Retry all method calls on certain exception.
 */
class RetryOnExceptionProxylicious[T, E <: Throwable](implicit manifest: ClassManifest[T], retryException: ClassManifest[E]) extends Proxylicious[T] {
  class RetryingSandwich extends Sandwich {
    def include(method: Method) = true
    def before(proxy: T, method: Method, args: Array[Any]) = ()
    def after(proxy: T, method: Method, args: Array[Any], result: Either[Throwable, Any]): Any = result match {
      case Right(r) ⇒ r
      case Left(e) ⇒
        if (retryException.erasure.isInstance(e)) {
          method.invoke(proxy, args.asInstanceOf[Array[Object]]: _*)
        } else {
          throw e
        }
    }
  }
  def proxy(meat: T): T = {
    proxy(meat, new RetryingSandwich)
  }
}