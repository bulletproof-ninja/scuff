import util.{ Random }
import scala.math._
import scala.concurrent.Future
import scala.util.{ Failure, Success, Try }
import scala.util.control.NonFatal
import java.math.MathContext
import scala.reflect.ClassTag

package object scuff {

  private[this] val UTF8 = java.nio.charset.Charset.forName("UTF-8")

  type TimedCache[K, V] = Cache[K, V] with Expiry[K, V]
  type Serializer[T] = Codec[T, Array[Byte]]

  implicit final class Require[A](private val a: A) extends AnyVal {
    def require(cond: A => Boolean): A = { Predef.require(cond(a)); a }
    def require(cond: A => Boolean, msg: => Any): A = { Predef.require(cond(a), msg); a }
  }

  implicit class ScuffOption[E](private val opt: Option[E]) extends AnyVal {
    def collectAs[S <: E: ClassTag]: Option[S] = opt.collect { case s: S => s }
    def collectOrElse[S <: E: ClassTag](orElse: => S): S = collectAs[S] getOrElse orElse
    def ||[F >: E](orElse: => F): F = opt getOrElse orElse
  }

  implicit class ScuffString(private val str: String) extends AnyVal {
    def optional: Option[String] = if (str == null || str.length == 0) None else Some(str)

    /** Calculate Levenshtein distance. */
    def levenshtein(s2: String): Int = new ScuffIdxSeq(str).levenshtein(s2)

    def unsafeInt(stopper: Numbers.Stopper = Numbers.NonStop, offset: Int = 0, length: Int = -1): Int = {
      Numbers.parseUnsafeInt(str, offset, end(offset, length))(stopper)
    }
    def utf8: Array[Byte] = str.getBytes(UTF8)
    @inline
    private def end(offset: Int, len: Int) = if (len == -1) str.length else offset + len
    def unsafeLong(stopper: Numbers.Stopper = Numbers.NonStop, offset: Int = 0, length: Int = -1): Long = {
      Numbers.parseUnsafeLong(str, offset, end(offset, length))(stopper)
    }

    def bd(implicit mc: MathContext = BigDecimal.defaultMathContext): BigDecimal = str match {
      case "0" => new BigDecimal(java.math.BigDecimal.ZERO, mc)
      case "1" => new BigDecimal(java.math.BigDecimal.ONE, mc)
      case _ => BigDecimal(str, mc)
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

  implicit class ScuffMap[A, B](private val map: Map[A, B]) extends AnyVal {
    def merge(other: Map[A, B])(collisionHandler: (B, B) => B): Map[A, B] =
      if (other.isEmpty) this.map
      else if (this.map.isEmpty) other
      else {
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
    def unsigned = Numbers unsigned b
  }
  implicit class ScuffShort(private val s: Short) extends AnyVal {
    def unsigned = Numbers unsigned s
  }
  implicit class ScuffLong(private val l: Long) extends AnyVal {
    def toByteArray = Numbers toBytes l
    def unsigned = Numbers unsigned l
  }
  implicit class ScuffInt(private val i: Int) extends AnyVal {
    def toByteArray = Numbers toBytes i
    def unsigned = Numbers unsigned i
  }
  implicit class ScuffByteArray(private val arr: Array[Byte]) extends AnyVal {
    def toLong(offset: Int = 0) = Numbers.bytesToLong(arr, offset)
    def toInt(offset: Int = 0) = Numbers.bytesToInt(arr, offset)
    def utf8: String = new String(arr, UTF8)
  }

  implicit class ScuffRandom(private val rand: Random) extends AnyVal {
    /** Next random number between inclusive/exclusive. */
    def nextBetween[T](inclExcl: (T, T))(implicit num: Numeric[T]): T = nextBetween(inclExcl._1, inclExcl._2)
    /** Next random number between inclusive/exclusive. */
    def nextBetween[T](incl: T, excl: T)(implicit num: Numeric[T]): T = {
      val scope = num.minus(excl, incl)
      val next = rand.nextDouble() * num.toDouble(scope) + num.toDouble(incl)
      num.zero match {
        case _: Double => next.asInstanceOf[T]
        case _: Int => next.toInt.asInstanceOf[T]
        case _: Long => next.toLong.asInstanceOf[T]
        case _: Float => next.asInstanceOf[Float].asInstanceOf[T]
        case _: BigDecimal => BigDecimal(next).asInstanceOf[T]
        case _: BigInt => BigInt(next.toInt).asInstanceOf[T]
        case _: Short => next.toShort.asInstanceOf[T]
        case _: Character => next.toChar.asInstanceOf[T]
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

  implicit class ScuffIterable[E](private val iter: Iterable[E]) extends AnyVal {
    def collectAs[S <: E: ClassTag]: Iterable[S] = iter.collect { case s: S => s }
    def last: E = lastOption || {
      throw new NoSuchElementException(s"${iter.getClass.getSimpleName}.last")
    }
    def lastOption: Option[E] = if (iter.isEmpty) None else iter match {
      case idxSeq: collection.IndexedSeq[E] => Some(idxSeq(idxSeq.length - 1))
      case _ =>
        var hasValue = false
        var value: E = null.asInstanceOf[E]
        iter.foreach { e =>
          value = e
          hasValue = true
        }
        if (hasValue) Some(value) else None
    }
    def head: E = headOption || {
      throw new NoSuchElementException(s"${iter.getClass.getSimpleName}.head")
    }
    def headOption: Option[E] = iter.find(_ => true)
  }
  implicit class ScuffIterator[E](private val iter: Iterator[E]) extends AnyVal {
    def collectAs[S <: E: ClassTag]: Iterator[S] = iter.collect { case s: S => s }
    def last: E = lastOption || {
      throw new NoSuchElementException(s"${iter.getClass.getSimpleName}.last")
    }
    def lastOption: Option[E] = if (iter.isEmpty) None else iter match {
      case idxSeq: collection.IndexedSeq[E] => Some(idxSeq(idxSeq.length - 1))
      case _ =>
        var hasValue = false
        var value: E = null.asInstanceOf[E]
        iter.foreach { e =>
          value = e
          hasValue = true
        }
        if (hasValue) Some(value) else None
    }
    def head: E = headOption || {
      throw new NoSuchElementException(s"${iter.getClass.getSimpleName}.head")
    }
    def headOption: Option[E] = iter.find(_ => true)

    /** This assumes a lazy iterator. */
    def limitActiveFutures(maxActiveFutures: Int)(pf: PartialFunction[E, Future[_]]): Iterator[E] = {
        val future: (E => Future[_]) = (e: E) => if (pf isDefinedAt e) pf(e) else Future.unit
      new concurrent.BlockingBoundedFuturesIterator(maxActiveFutures, iter, future)
    }
    /** This assumes a lazy iterator. */
    def limitActive(maxActiveFutures: Int)(implicit ev: E <:< Future[_]): Iterator[E] = {
      assert(ev != null) // No warning
      new concurrent.BlockingBoundedFuturesIterator[E](maxActiveFutures, iter, _.asInstanceOf[Future[_]])
    }
  }
  implicit class ScuffArray[E](private val arr: Array[E]) extends AnyVal {
    def levenshtein(s2: scala.collection.IndexedSeq[E]): Int = new ScuffIdxSeq(arr).levenshtein(s2)
  }
  implicit class ScuffIdxSeq[E](private val seq: scala.collection.IndexedSeq[E]) extends AnyVal {
    /**
      * Calculate Levenshtein distance.
      * Taken, and modified, from:
      * http://rosettacode.org/wiki/Levenshtein_distance#Scala
      */
    def levenshtein(s2: scala.collection.IndexedSeq[E]): Int = {
        def minimum(i1: Int, i2: Int, i3: Int) = min(min(i1, i2), i3)
      val dist = Array.tabulate(s2.length + 1, seq.length + 1) { (j, i) => if (j == 0) i else if (i == 0) j else 0 }
      for (j <- 1 to s2.length; i <- 1 to seq.length)
        dist(j)(i) = if (s2(j - 1) == seq(i - 1)) dist(j - 1)(i - 1)
        else minimum(dist(j - 1)(i) + 1, dist(j)(i - 1) + 1, dist(j - 1)(i - 1) + 1)

      dist(s2.length)(seq.length)
    }
  }

  implicit class ScuffBooleanFunction[I](private val f: I => Boolean) extends AnyVal {
    def negate: I => Boolean = (inp: I) => !f(inp)
  }

  implicit class ScuffJavaEnum[E <: java.lang.Enum[E]](private val e: E) extends AnyVal {
    def >(thatE: E): Boolean = this.e.ordinal > thatE.ordinal
    def >=(thatE: E): Boolean = this.e.ordinal >= thatE.ordinal
    def <(thatE: E): Boolean = this.e.ordinal < thatE.ordinal
    def <=(thatE: E): Boolean = this.e.ordinal <= thatE.ordinal
    def min(thatE: E): E = if (this.e.ordinal <= thatE.ordinal) this.e else thatE
    def max(thatE: E): E = if (this.e.ordinal >= thatE.ordinal) this.e else thatE
  }

  private[this] val ReturnNone = () => None
  private[this] val ReturnNil = () => Nil
  private[this] val ReturnUnit = () => ()
  private[this] val ReturnTrue = () => true
  private[this] val ReturnFalse = () => false

  implicit final class ScuffFunctionObject(private val f: Function.type) extends AnyVal {
    def none: () => None.type = ReturnNone
    def nil: () => Nil.type = ReturnNil
    def unit: () => Unit = ReturnUnit
    def True: () => Boolean = ReturnTrue
    def False: () => Boolean = ReturnFalse
  }

}
