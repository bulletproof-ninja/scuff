package scuff

import java.lang.reflect.{InvocationHandler, InvocationTargetException, Method, Proxy, UndeclaredThrowableException}

import language.implicitConversions
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

/**
 * Interface proxy factory.
 */
class Proxylicious[T](implicit tag: ClassTag[T]) {

  private[this] val getters = tag.runtimeClass.getMethods().filter(m => m.getParameterTypes.length == 0 && m.getReturnType != classOf[Unit]).toSeq

  private def newProxy(ih: InvocationHandler) = Proxy.newProxyInstance(tag.runtimeClass.getClassLoader, Array(tag.runtimeClass), ih).asInstanceOf[T]

  def proxify(handler: (T, Method, Array[AnyRef]) => Any): T = {
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
          case t: InvocationTargetException => Failure(t.getTargetException)
          case t: UndeclaredThrowableException => Failure(t.getUndeclaredThrowable)
          case t: Throwable => Failure(t)
        }
        if (include) {
          wrapper.after(proxy, method, anyArgs, result).asInstanceOf[Object]
        } else result match {
          case Failure(t) => throw t
          case Success(res) => res
        }
      }
    }
    newProxy(handler)
  }

  def withToStringOverride(meat: T, className: String = tag.runtimeClass.getSimpleName): T = sandwich(meat, newToStringOverrideSandwich(className))
  /**
   * Override equals/hashCode like a value object, i.e.
   * equality is based on all accessible values, not
   * identity. Same with hash code.
   */
  def withEqualsHashCodeOverride(meat: T): T = sandwich(meat, newEqualsHashCodeOverrideSandwich)

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

  private def newToStringOverrideSandwich(className: String): Sandwich = new Sandwich {
    def include(method: Method) = method.getParameterTypes.length == 0 && method.getName == "toString" && method.getReturnType == classOf[String]
    def before(proxy: T, method: Method, args: Array[Any]) {}
    def after(proxy: T, method: Method, args: Array[Any], result: Try[Any]): Any = {
      val sb = new java.lang.StringBuilder(getters.size * 16)
      sb append className append '('
      getters.foreach { getter =>
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

  private def newEqualsHashCodeOverrideSandwich: Sandwich = new Sandwich {
    def include(method: Method) =
      (isSingleObjectArg(method.getParameterTypes) && method.getName == "equals" && method.getReturnType == classOf[Boolean]) ||
        (method.getParameterTypes.length == 0 && method.getName == "hashCode" && method.getReturnType == classOf[Int])
    def before(proxy: T, method: Method, args: Array[Any]) {}
    def after(proxy: T, method: Method, args: Array[Any], result: Try[Any]): Any = {
      method.getName match {
        case "equals" =>
          args(0) match {
            case that: T => that != null && tag.runtimeClass.isInstance(that) && Try(getters.forall(m => m.invoke(proxy) == m.invoke(that))).getOrElse(false)
            case _ => false
          }
        case "hashCode" => getters.foldLeft(17) { (hash, m) =>
          m.invoke(proxy) match {
            case null => 31 * hash
            case value => 31 * hash + value.hashCode
          }
        }
        case _ => result match {
          case Success(res) => res
          case Failure(t) => throw t
        }
      }
    }

  }

}
