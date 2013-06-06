package scuff

import java.lang.reflect.{ InvocationHandler, Proxy, Method, UndeclaredThrowableException, InvocationTargetException }
import reflect.ClassTag

/**
 * Interface proxy factory.
 */
class Proxylicious[T](implicit manifest: ClassTag[T]) {

  private def newProxy(ih: InvocationHandler) = Proxy.newProxyInstance(manifest.runtimeClass.getClassLoader, Array(manifest.runtimeClass), ih).asInstanceOf[T]

  def proxify(handler: (T, Method, Array[AnyRef]) ⇒ Any): T = {
    this newProxy new InvocationHandler {
      def invoke(p: Any, method: Method, args: Array[AnyRef]) = handler(p.asInstanceOf[T], method, args).asInstanceOf[AnyRef]
    }
  }

  def sandwich(meat: T, wrapper: Sandwich): T = {
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
          case t: UndeclaredThrowableException ⇒ Left(t.getUndeclaredThrowable)
          case t: Exception ⇒ Left(t)
        }
        if (include) {
          wrapper.after(proxy, method, anyArgs, result).asInstanceOf[Object]
        } else result match {
          case Left(t) ⇒ throw t
          case Right(r) ⇒ r.asInstanceOf[AnyRef]
        }
      }
    }
    newProxy(handler)
  }
  trait Sandwich {
    class DSLMethod(method: Method) {
      def named(methodName: Symbol) = new DSLString(method, methodName.name)
    }
    class DSLString(method: Method, methodName: String = null) {
      def returns(returnType: Class[_]): Boolean = (methodName == null || nameMatches) && method.getReturnType() == returnType
      implicit def nameMatches() = method.getName() == methodName
    }
    implicit def method2dslm(method: Method) = new DSLMethod(method)
    implicit def method2dsls(method: Method) = new DSLString(method)
    implicit def dsl2bool(dsl: DSLString) = dsl.nameMatches

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

