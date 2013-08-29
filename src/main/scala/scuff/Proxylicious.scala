package scuff

import java.lang.reflect.{ InvocationHandler, Proxy, Method, UndeclaredThrowableException, InvocationTargetException }
import reflect.ClassTag
import scala.util.{ Try, Success, Failure }

/**
 * Interface proxy factory.
 */
class Proxylicious[T](implicit tag: ClassTag[T]) {

  private def newProxy(ih: InvocationHandler) = Proxy.newProxyInstance(tag.runtimeClass.getClassLoader, Array(tag.runtimeClass), ih).asInstanceOf[T]

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
        val result: Try[AnyRef] = try {
          Success(method.invoke(meat, args: _*))
        } catch {
          case t: InvocationTargetException ⇒ Failure(t.getTargetException)
          case t: UndeclaredThrowableException ⇒ Failure(t.getUndeclaredThrowable)
          case t: Throwable ⇒ Failure(t)
        }
        if (include) {
          wrapper.after(proxy, method, anyArgs, result).asInstanceOf[Object]
        } else result match {
          case Failure(t) ⇒ throw t
          case Success(res) ⇒ res
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
      def nameMatches() = method.getName() == methodName
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
    def after(proxy: T, method: Method, args: Array[Any], result: Try[Any]): Any
  }

  private def isSingleObjectArg(args: Array[Class[_]]) = args.length == 1 && args(0) == classOf[Object]

  def ToStringOverride: Sandwich = new Sandwich {
    def include(method: Method) = method.getParameterTypes.length == 0 && method.getReturnType == classOf[String] && method.getName == "toString"
    def before(proxy: T, method: Method, args: Array[Any]) {}
    def after(proxy: T, method: Method, args: Array[Any], result: Try[Any]): Any = {
      val sb = new StringBuffer
      sb append tag.runtimeClass.getSimpleName append '('
      val getters = tag.runtimeClass.getMethods().filter(_.getParameterTypes.length == 0)
      getters.foreach { getter ⇒
        sb append getter.getName append '=' append String.valueOf(getter.invoke(proxy)) append ','
      }
      if (getters.length > 0) {
        sb.setCharAt(sb.length - 1, ')')
      } else {
        sb append ')'
      }
      sb.toString
    }
  }

  /**
   * Override equals/hashCode like a value class, i.e.
   * equality is based on all accessible values, not
   * identity. Same with hash code.
   */
  def EqualsHashCodeOverride: Sandwich = new Sandwich {
    def include(method: Method) =
      (isSingleObjectArg(method.getParameterTypes) && method.getName == "equals" && method.getReturnType == classOf[Boolean]) ||
        (method.getParameterTypes.length == 0 && method.getName == "hashCode" && method.getReturnType == classOf[Int])
    def before(proxy: T, method: Method, args: Array[Any]) {}
    def after(proxy: T, method: Method, args: Array[Any], result: Try[Any]): Any = {
      val getters = tag.runtimeClass.getMethods().filter(_.getParameterTypes.length == 0)
      method.getName match {
        case "equals" ⇒ allEqual(proxy, args(0), getters)
        case "hashCode" ⇒ calcHash(proxy, getters)
        case _ ⇒ result match {
          case Success(res) ⇒ res
          case Failure(t) ⇒ throw t
        }
      }
    }
    private def allEqual(t1: T, t2: Any, getters: Seq[Method]): Boolean = getters.forall { m ⇒
      val v1 = m.invoke(t1)
      val v2 = m.invoke(t2)
      v1 == v2
    }
    private def calcHash(t: T, getters: Seq[Method]): Int = getters.foldLeft(17) { (hash, m) ⇒
      m.invoke(t) match {
        case null ⇒ 31 * hash
        case value ⇒ 31 * hash + value.hashCode
      }
    }

  }

}
