package scuff.reflect

import scala.reflect._
import java.lang.reflect.{ Array => _, _ }
import scala.util._
import scala.util.control.NoStackTrace

object DynamicConstructor {

  private type Key = (Class[_], Class[_])
  private type Factory = AnyRef => Any

  private[this] val cache = new scala.collection.concurrent.TrieMap[Key, Factory]

  def apply[T: ClassTag](any: Any): Option[T] = {
    val anyRef = any.asInstanceOf[AnyRef]
    val toType = classTag[T].runtimeClass.asInstanceOf[Class[T]]
    val key = anyRef.getClass -> toType
    cache.get(key) match {
      case Some(ctor) =>
        try {
          Option(ctor(anyRef).asInstanceOf[T])
        } catch {
          case _: IllegalArgumentException =>
            None
          case _: Exception =>
            cache.remove(key, ctor)
            apply(anyRef)
        }
      case _ =>
        construct(toType, anyRef).map {
          case (t, ctor) =>
            cache.putIfAbsent(key, ctor)
            t
        }
    }
  }

  @inline
  private def stringValueOf(anyRef: AnyRef): String = String.valueOf(anyRef)
  @inline
  private def invokeStatic[T](m: Method)(arg: AnyRef): T = m.invoke(null, arg).asInstanceOf[T]
  @inline
  private def invokeFactory[T](ctor: Constructor[_])(arg: AnyRef): T = ctor.newInstance(arg).asInstanceOf[T]

  private def construct[T](toType: Class[T], from: AnyRef): Option[(T, AnyRef => T)] = {
      def isParmTypeMatch(parmTypes: Array[Class[_]]) = {
        if (parmTypes.length != 1) {
          false
        } else if (parmTypes(0).isPrimitive) {
          primitiveToWrapper(parmTypes(0)).isInstance(from)
        } else {
          parmTypes(0).isInstance(from)
        }
      }
    if (toType == classOf[String]) Option(from).map { from =>
      val t = String.valueOf(from).asInstanceOf[T]
      val toT = (stringValueOf _).asInstanceOf[AnyRef => T]
      t -> toT
    }
    else {
      val toRefType = if (toType.isPrimitive) primitiveToWrapper(toType) else toType
      val ctors = toRefType.getConstructors.filter(ctor => isParmTypeMatch(ctor.getParameterTypes))
      val ctorSuccess = ctors.iterator.map(ctor => Try(invokeFactory[T](ctor)(from) -> ctor)).collectFirst {
        case Success((t, ctor)) => t.asInstanceOf[T] -> invokeFactory[T](ctor) _
      }
      ctorSuccess.orElse {
        val factoryMethods = toRefType.getMethods().filter { method =>
          Modifier.isStatic(method.getModifiers) &&
            isParmTypeMatch(method.getParameterTypes) && {
              toType.isAssignableFrom(method.getReturnType) ||
                toRefType.isAssignableFrom(method.getReturnType)
            }
        }
        val lazyInvoke = factoryMethods.iterator.map { m =>
          Try {
            invokeStatic[T](m)(from) match {
              case null => throw new NullPointerException with NoStackTrace
              case t => t -> invokeStatic(m) _
            }
          }
        }
        lazyInvoke.collectFirst {
          case Success(t) => t
        }
      }
    }
  }

}
