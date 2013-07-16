package scuff

import scala.collection.immutable.Range.Partial
import scala.collection.immutable.NumericRange
import java.lang.{ Double ⇒ JD, Float ⇒ JF }

final class Interval[T](
    val fromIncl: Boolean, val from: T,
    val toIncl: Boolean, val to: T,
    stringRep: String = null)(implicit private val n: Numeric[T]) {

  checkForNaN(from)
  checkForNaN(to)

  private def nanException = throw new IllegalArgumentException("Interval cannot contain NaN")
  private def checkForNaN(n: T) = n match {
    case d: Double if JD.isNaN(d) ⇒ nanException
    case f: Float if JF.isNaN(f) ⇒ nanException
    case _ ⇒ // All good
  }

  def contains(c: T) =
    (fromIncl && n.gteq(c, from) || n.gt(c, from)) &&
      (toIncl && n.lteq(c, to) || n.lt(c, to))

  private def openBracket = if (fromIncl) "[" else "("
  private def closeBracket = if (toIncl) "]" else ")"
  private def numStr(t: T): String = t match {
    case d: Double ⇒
      if (d == Double.PositiveInfinity) "∞"
      else if (d == Double.NegativeInfinity) "-∞"
      else d.toString
    case f: Float ⇒
      if (f == Float.PositiveInfinity) "∞"
      else if (f == Float.NegativeInfinity) "-∞"
      else f.toString
    case _ ⇒ t.toString
  }

  override def toString = if (stringRep != null) stringRep else
    "%s%s,%s%s".format(openBracket, numStr(from), numStr(to), closeBracket)

  override def equals(any: Any) = any match {
    case that: Interval[_] if this.n == that.n ⇒
      val that = any.asInstanceOf[Interval[T]]
      this.fromIncl == that.fromIncl &&
        this.toIncl == that.toIncl &&
        this.n.equiv(this.from, that.from) &&
        this.n.equiv(this.to, that.to)
  }
  override def hashCode = from.hashCode ^ to.hashCode
}

object Interval {

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
    DotRegex.findFirstMatchIn(str).orElse(CommaRegex.findFirstMatchIn(str)).map { m ⇒
      val fromIncl = m.group(1) == "["
      val toIncl = m.group(4) == "]"
      val from = BigDecimal(m.group(2).replace(',', '.'))
      val to = BigDecimal(m.group(3).replace(',', '.'))
      new Interval(fromIncl, from, toIncl, to, str)
    }
  } catch {
    case _: Exception ⇒ None
  }

  def apply[T](t: (T, T))(implicit n: Numeric[T]): Interval[T] = {
    new Interval(true, t._1, false, t._2)
  }
  def apply[T](r: Range): Interval[Int] = {
    new Interval(true, r.start, r.isInclusive, r.end)
  }
  def apply[T](r: NumericRange[T])(implicit n: Numeric[T]): Interval[T] = {
    val fromIncl = r.start match {
      case d: Double ⇒ !JD.isInfinite(d)
      case f: Float ⇒ !JF.isInfinite(f)
      case _ ⇒ true
    }
    val toIncl = r.end match {
      case d: Double if (JD.isInfinite(d)) ⇒ false
      case f: Float if (JF.isInfinite(f)) ⇒ false
      case _ ⇒ r.isInclusive
    }
    new Interval(fromIncl, r.start, toIncl, r.end)
  }
  def apply[T](partial: Range.Partial[T, NumericRange[T]])(implicit n: Numeric[T]): Interval[T] = {
    val range = try {
      partial.by(n.negate(n.one))
    } catch {
      case _: NumberFormatException ⇒ partial.by(n.one)
    }
    apply(range)
  }

  implicit def tuple[T](t: (T, T))(implicit n: Numeric[T]): Interval[T] = apply(t)
  implicit def range[T](r: Range): Interval[Int] = apply(r)
  implicit def numRange[T](r: NumericRange[T])(implicit n: Numeric[T]): Interval[T] = apply(r)
  implicit def partialRange[T](partial: Range.Partial[T, NumericRange[T]])(implicit n: Numeric[T]): Interval[T] = apply(partial)
}
