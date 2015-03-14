package scuff.util

import scuff.Proxylicious
import java.lang.reflect.Method
import scala.util._
/**
 * Retry all method calls on certain exception.
 */
class RetryOnExceptionProxylicious[T, E <: Throwable](implicit manifest: reflect.ClassTag[T], retryException: reflect.ClassTag[E]) extends Proxylicious[T] {
  class RetryingSandwich extends Sandwich {
    def include(method: Method) = true
    def before(proxy: T, method: Method, args: Array[Any]) = ()
    def after(proxy: T, method: Method, args: Array[Any], result: Try[Any]): Any = result match {
      case Success(r) => r
      case Failure(e) =>
        if (retryException.runtimeClass.isInstance(e)) {
          method.invoke(proxy, args.asInstanceOf[Array[Object]]: _*)
        } else {
          throw e
        }
    }
  }
  def withRetry(meat: T): T = {
    sandwich(meat, new RetryingSandwich)
  }
}