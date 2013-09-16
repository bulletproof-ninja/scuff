import java.util.Locale
import java.lang.reflect.Constructor
import java.lang.reflect.Modifier
import scala.reflect.ClassTag

package object scuff {
  import scala.math._

  implicit class ScuffString(val str: String) extends AnyVal {
    /**
     * Calculate Levenshtein distance.
     * Taken, and modified, from:
     * http://rosettacode.org/wiki/Levenshtein_distance#Scala
     */
    def levenshtein(s2: String): Int = {
        def minimum(i1: Int, i2: Int, i3: Int) = min(min(i1, i2), i3)
      val dist = Array.tabulate(s2.length + 1, str.length + 1) { (j, i) ⇒ if (j == 0) i else if (i == 0) j else 0 }
      for (j ← 1 to s2.length; i ← 1 to str.length)
        dist(j)(i) = if (s2(j - 1) == str(i - 1)) dist(j - 1)(i - 1)
        else minimum(dist(j - 1)(i) + 1, dist(j)(i - 1) + 1, dist(j - 1)(i - 1) + 1)

      dist(s2.length)(str.length)
    }

    /**
     * Length of string in Unicode code points.
     */
    def lengthUnicode(): Int = str.codePointCount(0, str.length)

  }

  implicit class ScuffLock(val lock: java.util.concurrent.locks.Lock) extends AnyVal {
    def whenLocked[T](code: ⇒ T): T = {
      lock.lock()
      try {
        code
      } finally {
        lock.unlock()
      }
    }
  }

  import scala.util.{ Try, Success, Failure }
  import concurrent.Future
  implicit class ScuffTry[T](val t: Try[T]) extends AnyVal {
    def toFuture: Future[T] = t match {
      case Success(res) ⇒ Future.successful(res)
      case Failure(e) ⇒ Future.failed(e)
    }
  }

  private val primitiveToWrapper: Map[Class[_], Class[_]] = Map(
    classOf[Boolean] -> classOf[java.lang.Boolean],
    classOf[Char] -> classOf[java.lang.Character],
    classOf[Short] -> classOf[java.lang.Short],
    classOf[Byte] -> classOf[java.lang.Byte],
    classOf[Int] -> classOf[java.lang.Integer],
    classOf[Long] -> classOf[java.lang.Long],
    classOf[Double] -> classOf[java.lang.Double],
    classOf[Float] -> classOf[java.lang.Float])

  private def coerce[T](from: AnyRef, toType: Class[T]): Option[T] = {
      def isParmTypeMatch(parmTypes: Array[Class[_]]) = {
        if (parmTypes.length != 1) {
          false
        } else if (parmTypes(0).isPrimitive) {
          val pt = primitiveToWrapper(parmTypes(0))
          pt.isInstance(from)
        } else {
          parmTypes(0).isInstance(from)
        }
      }
    if (toType == classOf[String]) Option(from).map(String.valueOf(_).asInstanceOf[T]) else {
      val ctors = toType.getConstructors.asInstanceOf[Array[Constructor[T]]].filter(ctor ⇒ isParmTypeMatch(ctor.getParameterTypes))
      val ctorSuccess = ctors.iterator.map(ctor ⇒ Try(ctor.newInstance(from))).collectFirst {
        case Success(t) ⇒ t
      }
      ctorSuccess.orElse {
        val factoryMethods = toType.getMethods().filter { method ⇒
          Modifier.isStatic(method.getModifiers) &&
            toType.isAssignableFrom(method.getReturnType) &&
            isParmTypeMatch(method.getParameterTypes)
        }
        val lazyInvoke = factoryMethods.iterator.map { m ⇒
          try {
            m.invoke(null, from).asInstanceOf[T] match {
              case null ⇒ Failure(new NullPointerException)
              case t ⇒ Success(t)
            }
          } catch {
            case e: Exception ⇒ Failure(e)
          }
        }
        lazyInvoke.collectFirst {
          case Success(t) ⇒ t
        }
      }
    }
  }

  implicit class ScuffAny(val any: Any) extends AnyVal {
    def coerceTo[T](implicit tag: ClassTag[T]): Option[T] = coerce[T](any.asInstanceOf[AnyRef], tag.runtimeClass.asInstanceOf[Class[T]])
  }

  implicit class ScuffMap[A, B](val map: Map[A, B]) extends AnyVal {
    def merge(other: Map[A, B], collisionHandler: (B, B) ⇒ B): Map[A, B] = {
      val merged = map.keySet.intersect(other.keySet).toSeq.map { key ⇒
        val merged = collisionHandler(map(key), other(key))
        key -> merged
      }.toMap
      map ++ other ++ merged
    }
    def intersectEquals(other: Map[A, B]): Boolean = {
      val intersection = map.keySet.intersect(other.keySet)
      intersection.nonEmpty && intersection.forall { key ⇒ map(key) == other(key) }
    }
  }

  implicit class ScuffLong(val l: Long) extends AnyVal {
    def toByteArray() = BitsBytes.longToBytes(l)
  }
  implicit class ScuffByteArray(val a: Array[Byte]) extends AnyVal {
    def toLong() = BitsBytes.bytesToLong(a)
  }

}
