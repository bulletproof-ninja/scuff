import scala.collection.immutable.NumericRange
import util.{ Random }
import scala.math._
import scala.concurrent.Future
import scala.util.{ Failure, Success, Try }
import scala.collection.GenTraversableOnce
import scala.util.control.NonFatal

package object scuff {

  private[this] val UTF8 = java.nio.charset.Charset.forName("UTF-8")

  type TimedCache[K, V] = Cache[K, V] with Expiry[K, V]
  type Serializer[T] = Codec[T, Array[Byte]]

  implicit class ScuffString(private val str: String) extends AnyVal {
    def optional: Option[String] = if (str == null || str.length == 0) None else Some(str)

    /**
      * Calculate Levenshtein distance.
      * Taken, and modified, from:
      * http://rosettacode.org/wiki/Levenshtein_distance#Scala
      */
    def levenshtein(s2: String): Int = {
        def minimum(i1: Int, i2: Int, i3: Int) = min(min(i1, i2), i3)
      val dist = Array.tabulate(s2.length + 1, str.length + 1) { (j, i) => if (j == 0) i else if (i == 0) j else 0 }
      for (j <- 1 to s2.length; i <- 1 to str.length)
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

  implicit class ScuffTry[T](private val t: Try[T]) extends AnyVal {
    def toFuture: Future[T] = t match {
      case Success(res) => Future.successful(res)
      case Failure(e) => Future.failed(e)
    }
  }

  implicit class ScuffAny[A](private val any: A) extends AnyVal {
    def optional(some: Boolean): Option[A] = if (some) Option(any) else None
  }

  implicit class ScuffMap[A, B](private val map: Map[A, B]) extends AnyVal {
    def merge(other: Map[A, B])(collisionHandler: (B, B) => B): Map[A, B] = {
      val merged = map.keySet.intersect(other.keySet).toSeq.map { key =>
        val merged = collisionHandler(map(key), other(key))
        key -> merged
      }.toMap
      map ++ other ++ merged
    }
    def intersectEquals(other: Map[A, B]): Boolean = {
      val intersection = map.keySet.intersect(other.keySet)
      intersection.nonEmpty && intersection.forall { key => map(key) == other(key) }
    }
  }

  implicit class ScuffByte(private val b: Byte) extends AnyVal {
    def unsigned() = Numbers.unsigned(b)
  }
  implicit class ScuffShort(private val s: Short) extends AnyVal {
    def unsigned() = Numbers.unsigned(s)
  }
  implicit class ScuffLong(private val l: Long) extends AnyVal {
    def toByteArray() = Numbers.longToBytes(l)
    def unsigned() = Numbers.unsigned(l)
  }
  implicit class ScuffInt(private val i: Int) extends AnyVal {
    def toByteArray() = Numbers.intToBytes(i)
    def unsigned() = Numbers.unsigned(i)
  }
  implicit class ScuffByteArray(private val arr: Array[Byte]) extends AnyVal {
    def toLong(offset: Int = 0) = Numbers.bytesToLong(arr, offset)
    def toInt(offset: Int = 0) = Numbers.bytesToInt(arr, offset)
    def utf8: String = new String(arr, UTF8)
  }

  implicit class ScuffRandom(private val rand: Random) extends AnyVal {
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
        case _: Short => math.round(next).asInstanceOf[T]
        case _: Character => math.round(next).asInstanceOf[Char].asInstanceOf[T]
        case _ => throw new IllegalArgumentException(s"${num.zero.getClass.getName} is unsupported")
      }
    }
  }

  implicit class ScuffURI(private val uri: java.net.URI) extends AnyVal {
    def openInBrowser(): Boolean = try {
      if (java.awt.Desktop.isDesktopSupported) {
        java.awt.Desktop.getDesktop.browse(uri)
        true
      } else false
    } catch {
      case NonFatal(_) => false
    }
  }
  implicit class ScuffURL(private val url: java.net.URL) extends AnyVal {
    def openInBrowser(): Boolean = url.toURI.openInBrowser()
  }

  implicit class ScuffTraversableOnce[E](private val trav: GenTraversableOnce[E]) extends AnyVal {
    def last: E = try trav.reduce((_, e) => e) catch {
      case _: UnsupportedOperationException => throw new NoSuchElementException(s"${trav.getClass.getSimpleName}.last")
    }
    def lastOption: Option[E] = trav.reduceOption((_, e) => e)
  }
  implicit class ScuffTraversable[Trav <: Traversable[_]](private val trav: Trav) extends AnyVal {
    def optional: Option[Trav] = if (trav == null || trav.isEmpty) None else Some(trav)
  }

  implicit class ScuffBooleanFunction[I](private val f: I => Boolean) extends AnyVal {
    def negate: I => Boolean = (inp: I) => !f(inp)
  }

  implicit class ScuffJavaEnum[E <: Enum[E]](private val enum: E) extends AnyVal {
    def >(thatEnum: E): Boolean = this.enum.ordinal > thatEnum.ordinal
    def >=(thatEnum: E): Boolean = this.enum.ordinal >= thatEnum.ordinal
    def <(thatEnum: E): Boolean = this.enum.ordinal < thatEnum.ordinal
    def <=(thatEnum: E): Boolean = this.enum.ordinal <= thatEnum.ordinal
    def min(thatEnum: E): E = if (this.enum.ordinal <= thatEnum.ordinal) this.enum else thatEnum
    def max(thatEnum: E): E = if (this.enum.ordinal >= thatEnum.ordinal) this.enum else thatEnum
  }
}
