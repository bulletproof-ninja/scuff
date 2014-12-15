import java.lang.reflect.{ Constructor, Modifier }

import scala.Range
import scala.collection.immutable.NumericRange
import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.Duration
import scala.language.implicitConversions
import scala.math.{ BigDecimal, BigInt, Numeric, min }
import scala.reflect.ClassTag
import scala.util.{ Failure, Random, Success, Try }

import scuff.{ Cache, Codec, Expiry, Numbers, Threads }

package object scuff {
  import scala.math._

  private[this] val UTF8 = java.nio.charset.Charset.forName("UTF-8")

  type TimedCache[K, V] = Cache[K, V] with Expiry[K, V]
  type Serializer[T] = Codec[T, Array[Byte]]

  implicit final class ScuffString(val str: String) extends AnyVal {
    def optional: Option[String] = if (str.length == 0) None else Some(str)

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

    def unsafeInt(stopper: Numbers.Stopper = Numbers.NonStop, offset: Int = 0, length: Int = -1): Int = {
      Numbers.parseUnsafeInt(str, offset, end(offset, length))(stopper)
    }
    def utf8: Array[Byte] = str.getBytes(UTF8)
    @inline
    private def end(offset: Int, len: Int) = if (len == -1) str.length else offset + len
    def unsafeLong(stopper: Numbers.Stopper = Numbers.NonStop, offset: Int = 0, length: Int = -1): Long = {
      Numbers.parseUnsafeLong(str, offset, end(offset, length))(stopper)
    }

    def offsetStartsWith(offset: Int, startsWith: CharSequence): Boolean = {
        @annotation.tailrec
        def allCharsEqual(thisOffset: Int, thatOffset: Int): Boolean = {
          if (thatOffset < startsWith.length) {
            if (str.charAt(thisOffset) == startsWith.charAt(thatOffset)) {
              allCharsEqual(thisOffset + 1, thatOffset + 1)
            } else {
              false
            }
          } else {
            true
          }
        }
      str.length - offset >= startsWith.length && allCharsEqual(offset, 0)
    }

  }

  implicit final class ScuffLock(val lock: java.util.concurrent.locks.Lock) extends AnyVal {
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
  implicit final class ScuffTry[T](val t: Try[T]) extends AnyVal {
    def toFuture: Future[T] = t match {
      case Success(res) ⇒ Future.successful(res)
      case Failure(e) ⇒ Future.failed(e)
    }
  }

  private[scuff] val primitiveToWrapper: Map[Class[_], Class[_]] = Map(
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

  implicit final class ScuffAny(val any: Any) extends AnyVal {
    def coerceTo[T](implicit tag: ClassTag[T]): Option[T] = coerce[T](any.asInstanceOf[AnyRef], tag.runtimeClass.asInstanceOf[Class[T]])
  }

  implicit final class ScuffMap[A, B](val map: Map[A, B]) extends AnyVal {
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

  implicit final class ScuffByte(val b: Byte) extends AnyVal {
    def unsigned() = Numbers.unsigned(b)
  }
  implicit final class ScuffShort(val s: Short) extends AnyVal {
    def unsigned() = Numbers.unsigned(s)
  }
  implicit final class ScuffLong(val l: Long) extends AnyVal {
    def toByteArray() = Numbers.longToBytes(l)
    def unsigned() = Numbers.unsigned(l)
  }
  implicit final class ScuffInt(val i: Int) extends AnyVal {
    def toByteArray() = Numbers.intToBytes(i)
    def unsigned() = Numbers.unsigned(i)
  }
  implicit final class ScuffByteArray(val arr: Array[Byte]) extends AnyVal {
    def toLong(offset: Int = 0) = Numbers.bytesToLong(arr, offset)
    def toInt(offset: Int = 0) = Numbers.bytesToInt(arr, offset)
    def utf8: String = new String(arr, UTF8)
  }

  implicit final class ScuffArray[T](val arr: Array[T]) extends AnyVal {
    private def NoSuchElement = new NoSuchElementException(s"Array.length = ${arr.length}")
    def take2(): (T, T) = if (arr.length >= 2) arr(0) -> arr(1) else throw NoSuchElement
    def take3(): (T, T, T) = if (arr.length >= 3) (arr(0), arr(1), arr(2)) else throw NoSuchElement
    def take4(): (T, T, T, T) = if (arr.length >= 4) (arr(0), arr(1), arr(2), arr(3)) else throw NoSuchElement
  }

  implicit final class ScuffRandom(val rand: Random) extends AnyVal {
    def nextInRange(r: Range): Int = nextInRange(NumericRange.inclusive(r.head, r.last, 1))
    def nextInRange[T](r: Range.Partial[T, NumericRange[T]])(implicit num: Numeric[T]): T = nextInRange(r.by(num.one))
    def nextInRange[T](r: NumericRange[T])(implicit num: Numeric[T]): T = {
      val width = num.minus(r.last, r.head)
      val next = rand.nextDouble * num.toDouble(width) + num.toDouble(r.head)
      num.zero match {
        case _: Double => next.asInstanceOf[T]
        case _: Int => math.round(next).asInstanceOf[Int].asInstanceOf[T]
        case _: Long => math.round(next).asInstanceOf[T]
        case _: Float => next.asInstanceOf[Float].asInstanceOf[T]
        case _: BigDecimal => BigDecimal(next).asInstanceOf[T]
        case _: BigInt => BigInt(math.round(next)).asInstanceOf[T]
        case _ => throw new IllegalArgumentException(s"${num.zero.getClass.getName} is unsupported")
      }
    }
  }

  implicit final class ScuffScalaFuture[T](val f: concurrent.Future[T]) extends AnyVal {
    def get(implicit maxWait: Duration): T =
      if (f.isCompleted) {
        f.value.get.get
      } else {
        Await.result(f, maxWait)
      }
  }

  implicit final class ScuffJavaFuture[T](val f: java.util.concurrent.Future[T]) extends AnyVal {
    def asScala(implicit conv: java.util.concurrent.Future[T] => concurrent.Future[T] = Threads.javaFutureConverter): concurrent.Future[T] = conv(f)
  }

  implicit final class ScuffTraversable[T <: Traversable[_]](val t: T) extends AnyVal {
    def optional: Option[T] = if (t.isEmpty) None else Some(t)
  }

}
