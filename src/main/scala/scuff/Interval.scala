package scuff

import scala.collection.immutable.NumericRange
import java.lang.{ Double => JD, Float => JF }

final class Interval[@specialized(Short, Int, Long, Float, Double) T](
    val fromIncl: Boolean, val from: T,
    val toIncl: Boolean, val to: T,
    stringRep: String = null)(implicit private val ord: Ordering[T]) extends Serializable {

  checkForNaN(from)
  checkForNaN(to)

  private def nanException = throw new IllegalArgumentException("Interval cannot contain NaN")
  private def checkForNaN(n: T) = n match {
    case d: Double if JD.isNaN(d) => nanException
    case f: Float if JF.isNaN(f) => nanException
    case _ => // All good
  }

  def toStream(step: T)(implicit integral: Integral[T]): Stream[T] = {
    val start = if (fromIncl) from else integral.plus(from, integral.one)
    val end = if (toIncl) to else integral.minus(to, integral.one)
    Stream.range(start, end, step)
  }
  def toStream(implicit integral: Integral[T]): Stream[T] = toStream(integral.one)

  def contains(c: T) =
    (fromIncl && ord.gteq(c, from) || ord.gt(c, from)) &&
      (toIncl && ord.lteq(c, to) || ord.lt(c, to))

  def overlaps(that: Interval[T]): Boolean =
    (ord.gt(this.to, that.from) || (this.toIncl && that.fromIncl && ord.equiv(this.to, that.from))) &&
      (ord.lt(this.from, that.to) || (this.fromIncl && that.toIncl && ord.equiv(this.from, that.to)))

  private def openBracket = if (fromIncl) "[" else "("
  private def closeBracket = if (toIncl) "]" else ")"
  private def numStr(t: T): String = t match {
    case d: Double =>
      if (d == Double.PositiveInfinity) "∞"
      else if (d == Double.NegativeInfinity) "-∞"
      else String valueOf d
    case f: Float =>
      if (f == Float.PositiveInfinity) "∞"
      else if (f == Float.NegativeInfinity) "-∞"
      else String valueOf f
    case _ => String valueOf t
  }

  override def toString = if (stringRep != null) stringRep else
    s"$openBracket${numStr(from)},${numStr(to)}$closeBracket"

  override def equals(that: Any) = that match {
    case that: Interval[T] => (this eq that) || {
      this.ord == that.ord &&
        this.fromIncl == that.fromIncl &&
        this.toIncl == that.toIncl &&
        this.ord.equiv(this.from, that.from) &&
        this.ord.equiv(this.to, that.to)
    }
    case _ => false
  }
  override def hashCode = from.hashCode ^ to.hashCode

  private def writeObject(out: java.io.ObjectOutputStream): Unit = {
    out.writeBoolean(fromIncl)
    out.writeBoolean(toIncl)
    out.writeObject(from)
    out.writeObject(to)
    out.writeObject(ord)
    out.writeObject(stringRep)
  }
  private def readObject(in: java.io.ObjectInputStream): Unit = {
    val surgeon = new reflect.Surgeon(this)
    surgeon.set('fromIncl, in.readBoolean)
    surgeon.set('toIncl, in.readBoolean)
    surgeon.set('from, in.readObject)
    surgeon.set('to, in.readObject)
    surgeon.set(Interval.field_ord_name, in.readObject)
    surgeon.set('stringRep, in.readObject)
  }

}

object Interval {

  private val field_ord_name = Symbol(classOf[Interval[_]].getDeclaredFields().find(f => classOf[Ordering[_]] == f.getType).get.getName)

  val Unbounded = new Interval(false, Double.NegativeInfinity, false, Double.PositiveInfinity)

  private final val OpenBrackets = """(\[|\]|\()"""
  private final val CloseBrackets = """(\[|\]|\))"""
  private final val DotNumber = """(\d+(?:\.\d*)?)"""
  private final val DotNumberSeparator = """\s*[,;]\s*"""
  private final val CommaNumber = """(\d+(?:,\d*)?)"""
  private final val CommaNumberSeparator = """\s*;\s*"""
  private final val DotNotation = "^" + OpenBrackets + DotNumber + DotNumberSeparator + DotNumber + CloseBrackets + "$"
  private final val CommaNotation = "^" + OpenBrackets + CommaNumber + CommaNumberSeparator + CommaNumber + CloseBrackets + "$"
  private val DotRegex = DotNotation.r
  private val CommaRegex = CommaNotation.r

  def parse(str: String): Option[Interval[BigDecimal]] = try {
    DotRegex.findFirstMatchIn(str).orElse(CommaRegex.findFirstMatchIn(str)).map { m =>
      val fromIncl = m.group(1) == "["
      val toIncl = m.group(4) == "]"
      val from = BigDecimal(m.group(2).replace(',', '.'))
      val to = BigDecimal(m.group(3).replace(',', '.'))
      new Interval(fromIncl, from, toIncl, to, str)
    }
  } catch {
    case _: Exception => None
  }

  def inclExcl[@specialized(Short, Int, Long, Float, Double) T](t: (T, T))(implicit n: Ordering[T]): Interval[T] = {
    new Interval(true, t._1, false, t._2)
  }
  def inclIncl[@specialized(Short, Int, Long, Float, Double) T](t: (T, T))(implicit n: Ordering[T]): Interval[T] = {
    new Interval(true, t._1, true, t._2)
  }
  def exclIncl[@specialized(Short, Int, Long, Float, Double) T](t: (T, T))(implicit n: Ordering[T]): Interval[T] = {
    new Interval(false, t._1, true, t._2)
  }
  def exclExcl[@specialized(Short, Int, Long, Float, Double) T](t: (T, T))(implicit n: Ordering[T]): Interval[T] = {
    new Interval(false, t._1, false, t._2)
  }
  def apply(r: Range): Interval[Int] = {
    new Interval(true, r.start, r.isInclusive, r.end)
  }
  def apply[@specialized(Short, Int, Long, Float, Double) T](r: NumericRange[T])(implicit n: Ordering[T]): Interval[T] = {
    val fromIncl = r.start match {
      case d: Double => !JD.isInfinite(d)
      case f: Float => !JF.isInfinite(f)
      case _ => true
    }
    val toIncl = r.end match {
      case d: Double if (JD.isInfinite(d)) => false
      case f: Float if (JF.isInfinite(f)) => false
      case _ => r.isInclusive
    }
    new Interval(fromIncl, r.start, toIncl, r.end)
  }
  def apply[@specialized(Short, Int, Long, Float, Double) T](partial: Range.Partial[T, NumericRange[T]])(implicit n: Numeric[T]): Interval[T] = {
    val range = try {
      partial.by(n.negate(n.one))
    } catch {
      case _: NumberFormatException => partial.by(n.one)
    }
    apply(range)
  }

  import language.implicitConversions

  implicit def tuple[@specialized(Short, Int, Long, Float, Double) T](t: (T, T))(implicit n: Ordering[T]): Interval[T] = inclExcl(t)
  implicit def range(r: Range): Interval[Int] = apply(r)
  implicit def numRange[@specialized(Short, Int, Long, Float, Double) T](r: NumericRange[T])(implicit n: Ordering[T]): Interval[T] = apply(r)
  implicit def partialRange[@specialized(Short, Int, Long, Float, Double) T](partial: Range.Partial[T, NumericRange[T]])(implicit n: Numeric[T]): Interval[T] = apply(partial)
}
