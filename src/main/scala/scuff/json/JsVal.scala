package scuff.json

import java.lang.reflect.Modifier
import java.lang.reflect.Method

import scala.jdk.CollectionConverters._
import java.math.MathContext
import scala.reflect.{ ClassTag, classTag }
import java.beans.Introspector
import java.math.{ BigInteger => JBigInt, BigDecimal => JBigDec }

import language.dynamics
import java.util.UUID
import java.time.temporal.Temporal
import scuff.EmailAddress

sealed abstract class JsVal {
  final def getOrElse[JS <: JsVal: ClassTag](orElse: => JS): JS =
    if (classTag[JS].runtimeClass isInstance this) this.asInstanceOf[JS]
    else orElse
  def toJson(implicit config: JsVal.Config): String
  private[this] def wrongType(expected: Class[_]) =
    sys.error(s"Not ${expected.getSimpleName}: $this")
  def asNum: JsNum = wrongType(classOf[JsNum])
  def asBool: JsBool = wrongType(classOf[JsBool])
  def asStr: JsStr = wrongType(classOf[JsStr])
  def asObj: JsObj = wrongType(classOf[JsObj])
  def asArr: JsArr = wrongType(classOf[JsArr])
}
case class JsNum(value: Number) extends JsVal {
  override def asNum = this
  override def asStr: JsStr = new JsStr(String valueOf value)
  def toJson(implicit config: JsVal.Config): String = {
    if (value == null) JsNull.toJson
    else value.toString match {
      case asString @ ("NaN" | "Infinity" | "-Infinity") => s""""$asString""""
      case numString =>
        if (numString endsWith ".0") numString.substring(0, numString.length - 2)
        else numString
    }
  }
  def toByte = value.byteValue
  def toInt = value.intValue
  def toShort = value.shortValue
  def toLong = value.longValue
  def toFloat = value.floatValue
  def toDouble = value.doubleValue
  def toBigInt: BigInt = value match {
    case bd: JBigDec => BigInt(bd.toBigInteger)
    case bi: JBigInt => BigInt(bi)
    case bd: BigDecimal => bd.toBigInt
    case bi: BigInt => bi
    case num => BigInt(num.toString)
  }
  def toBigDec(mc: MathContext = MathContext.DECIMAL128): BigDecimal = value match {
    case bd: JBigDec => new BigDecimal(bd, mc)
    case bi: JBigInt => new BigDecimal(new JBigDec(bi, mc), mc)
    case bd: BigDecimal =>
      if (mc == bd.mc) bd
      else bd(mc)
    case bi: BigInt => BigDecimal(bi, mc)
    case num => BigDecimal(num.toString, mc)
  }

  override def hashCode: Int = this.value.intValue.##
  override def equals(that: Any) = (this eq that.asInstanceOf[AnyRef]) || {
    that match {
      case JsNum(`value`) => true
      case JsNum(bd: JBigDec) => numberEquals(BigDecimal(bd))
      case JsNum(bi: JBigInt) => numberEquals(BigInt(bi))
      case JsNum(number) => numberEquals(number)
      case _ => false
    }
  }
  private def numberEquals(thatValue: Number): Boolean = {
    val thisValue = this.value match {
      case bd: JBigDec => BigDecimal(bd)
      case bi: JBigInt => BigInt(bi)
      case n => n
    }
    thisValue == thatValue
  }

}

object JsNum {

  val NaN = new JsNum(Double.NaN)
  val PositiveInfinity = new JsNum(Double.PositiveInfinity)
  val NegativeInfinity = new JsNum(Double.NegativeInfinity)
  val Zero: JsNum = new JsNum(0L) {
    override def toBigDec(mc: MathContext) = implicitly[Numeric[BigDecimal]].zero
  }
  val One: JsNum = new JsNum(1L) {
    override def toBigDec(mc: MathContext) = implicitly[Numeric[BigDecimal]].one
  }

  private[this] val long2JsNum =
    (Byte.MinValue to Byte.MaxValue).foldLeft(new Array[JsNum](256)) {
      case (arr, num) =>
        arr(num+128) = new JsNum(num)
        arr
    }

  def apply(l: Long): JsNum =
    if (l >= Byte.MinValue && l <= Byte.MaxValue) long2JsNum(l.asInstanceOf[Int]+128)
    else new JsNum(l)

  def apply(d: Double): JsNum =
    if (d.isNaN) NaN
    else if (d.isPosInfinity) PositiveInfinity
    else if (d.isNegInfinity) NegativeInfinity
    else new JsNum(d)

}

final case class JsStr(value: String) extends JsVal {
  override def asStr = this
  override def asNum = JsStr.String2JsNum(value)
  def toJson(implicit config: JsVal.Config) =
    if (value == null) JsNull.toJson
    else s""""${JsStr.escape(value)}""""
}
object JsStr {

  private val String2JsNum = {
    import java.math.{ BigDecimal => BD }
    val cache = {
      (Byte.MinValue to Byte.MaxValue).foldLeft(Map.empty[String, JsNum]) {
        case (map, num) => map.updated(num.toString, JsNum.apply(num: Long))
      } ++
      Map("NaN" -> JsNum.NaN, "Infinity" -> JsNum.PositiveInfinity, "-Infinity" -> JsNum.NegativeInfinity)
    }
    cache.withDefault { numStr =>
      val num: Number = try new BD(numStr) catch {
        case _: NumberFormatException =>
          java.lang.Double.parseDouble(numStr)
      }
      new JsNum(num)
    }
  }
  private[json] def escape(inp: String)(implicit config: JsVal.Config): String =
    escape(inp, config.escapeSlash, config.upperCaseHex)

  private def escape(inp: String, escapeSlash: Boolean, upperCaseHex: Boolean): String = {

      @inline def toHex(ch: Char): String = {
        if (upperCaseHex) ch.toHexString.toUpperCase
        else ch.toHexString
      }

      def escape(out: java.lang.StringBuilder, idx: Int): String = {
        if (idx == inp.length) out.toString()
        else {
          (inp charAt idx) match {
            case '"' => out append '\\' append '"'
            case '\b' => out append '\\' append 'b'
            case '\f' => out append '\\' append 'f'
            case '\n' => out append '\\' append 'n'
            case '\r' => out append '\\' append 'r'
            case '\t' => out append '\\' append 't'
            case '\\' => out append '\\' append '\\'
            case '/' if escapeSlash => out append '\\' append '/'
            case ch if ch > 0xfff => out append "\\u" append toHex(ch)
            case ch if ch > 0xff => out append "\\u0" append toHex(ch)
            case ch if Character isISOControl ch =>
              if (ch > 0xf) out append "\\u00" append toHex(ch)
              else out append "\\u000" append toHex(ch)
            case ch => out append ch
          }
          escape(out, idx + 1)
        }
      }

    escape(new java.lang.StringBuilder(inp.length * 2), 0)
  }

  def toJson(inp: String, escapeSlash: Boolean = false, upperCaseHex: Boolean = false): String =
    s""""${escape(inp, escapeSlash, upperCaseHex)}""""

}

final case object JsNull extends JsVal {
  def toJson(implicit config: JsVal.Config) = "null"
}
final case object JsUndefined extends JsVal {
  // Not supposed to be serialized, but probably better to emit "null" than fail
  def toJson(implicit config: JsVal.Config) = "null"
}

final case class JsObj(
  props: Map[String, JsVal])(
  implicit
  config: JsVal.Config)
extends JsVal
with Iterable[(String, JsVal)]
with Dynamic {

  def this(props: (String, JsVal)*)(
      implicit
      config: JsVal.Config) =
    this(props.toMap)

  override def asObj = this
  def toJson(implicit config: JsVal.Config) =
    if (props == null) JsNull.toJson
    else props.iterator.map {
      case (name, value) => s""""${JsStr.escape(name)}":${value.toJson}"""
    }.mkString("{", ",", "}")
  def get(name: String): Option[JsVal] =
    if (props != null) props.get(name) else None
  def getOrElse(name: String, default: => JsVal) =
    if (props != null) props.getOrElse(name, default) else default
  def apply(name: String): JsVal =
    if (props != null) {
      props.getOrElse(name, config.undefinedAccess(name))
    } else config.undefinedAccess(name)
  def selectDynamic(name: String): JsVal = apply(name)
  def updated(name: String, value: JsVal): JsObj =
    JsObj(props.updated(name, value))
  def iterator = if (props != null) props.iterator else Iterator.empty
}
object JsObj {
  def Empty(implicit config: JsVal.Config) = JsObj()
  def apply(
      props: (String, JsVal)*)(
      implicit
      config: JsVal.Config): JsObj =
    new JsObj(props.toMap)
}
final case class JsArr(
  values: JsVal*)(
  implicit
  config: JsVal.Config)
extends JsVal
with Iterable[JsVal] {
  override def asArr = this
  def toJson(implicit config: JsVal.Config) = values.iterator.map(_.toJson).mkString("[", ",", "]")
  def get(idx: Int): Option[JsVal] = if (idx >= 0 && idx < values.size) Some(values(idx)) else None
  def apply(idx: Int): JsVal =
    if (idx >= 0 && idx < values.size) values(idx)
    else config.undefinedAccess(idx)
  def iterator = values.iterator
  def length = values.size
}
object JsArr {
  def Empty(implicit config: JsVal.Config) = JsArr()
}
final case class JsBool(value: Boolean) extends JsVal {
  override def asBool = this
  def toJson(implicit config: JsVal.Config) = if (value) "true" else "false"
}
object JsBool {
  val True = JsBool(true)
  val False = JsBool(false)
}

object JsVal {

  /**
   * @param escapeSlash Escape the character `/` as `\/` in strings?
   * @param upperCaseHex Output hexadecimal digits in upper case?
   */
  case class Config(
      escapeSlash: Boolean = false,
      upperCaseHex: Boolean = false,
      undefinedAccess: Any => JsVal = _ => JsUndefined) {

    def withUndefinedAccess(onUndefined: Any => JsVal): Config =
      this.copy(undefinedAccess = onUndefined)

  }

  implicit val DefaultConfig = new Config()

  implicit def toJsVal(str: String): JsVal = if (str == null) JsNull else JsStr(str)
  implicit def toJsVal(num: Number): JsVal = if (num == null) JsNull else JsNum(num)
  implicit def toJsVal(num: Long): JsVal = JsNum(num: Number)
  implicit def toJsVal(num: Double): JsVal = JsNum(num: Number)
  implicit def toJsVal(b: Boolean): JsVal = if (b) JsBool.True else JsBool.False
  implicit def toJsVal(m: Map[String, Any]): JsVal = if (m == null) JsNull else JsObj(m.map(e => e._1 -> JsVal(e._2)))
  implicit def toJsVal(a: Iterable[Any]): JsVal = if (a == null) JsNull else JsArr(a.iterator.map(JsVal(_)).toSeq: _*)
  implicit def toJsVal(t: (String, Any)): (String, JsVal) = t._1 -> JsVal(t._2)

  implicit def stringValue(js: JsStr): String = js.value
  implicit def toByte(js: JsNum): Short = js.toByte
  implicit def toShort(js: JsNum): Short = js.toShort
  implicit def toInt(js: JsNum): Int = js.toInt
  implicit def toLong(js: JsNum): Long = js.toLong
  implicit def toFloat(js: JsNum): Float = js.toFloat
  implicit def toDouble(js: JsNum): Double = js.toDouble
  implicit def toBigDecimal(js: JsNum): BigDecimal = js.toBigDec()
  implicit def toJavaBigDecimal(js: JsNum): java.math.BigDecimal = js.toBigDec().underlying
  implicit def boolValue(js: JsBool): Boolean = js.value

  def apply(any: Any, mapper: PartialFunction[Any, Any] = PartialFunction.empty): JsVal = any match {
    case jv: JsVal => jv
    case any if mapper isDefinedAt any => JsVal(mapper(any), mapper)
    case null => JsNull
    case n: Number => JsNum(n)
    case s: String => JsStr(s)
    case b: Boolean => JsBool(b)
    case m: collection.Map[_, _] => JsObj {
      m.iterator.filterNot(_._2 == JsUndefined).map {
        case (key, value) => String.valueOf(key) -> JsVal(value, mapper)
      }.toMap
    }
    case o: Option[_] => o match {
      case Some(value) => JsVal(value, mapper)
      case _ => JsNull
    }
    case e: Either[_, _] => e match {
      case Right(value) => JsVal(value, mapper)
      case Left(value) => JsVal(value, mapper)
    }
    case i: Iterable[_] => JsArr(i.iterator.map(JsVal(_, mapper)).toSeq: _*)
    case m: java.util.Map[_, _] => JsObj {
      m.asScala.iterator.filterNot(_._2 == JsUndefined).map {
        case (key, value) => String.valueOf(key) -> JsVal(value, mapper)
      }.toMap
    }
    case i: java.lang.Iterable[_] => JsArr(i.iterator.asScala.map(JsVal(_, mapper)).toSeq: _*)
    case asString @ (_: UUID | _: Temporal | _: EmailAddress) => JsStr(asString.toString)
    case other =>
      val methods = other match {
        case _: AnyRef => getters.get(other.getClass)
        case _ => Nil
      }
      if (methods.isEmpty) JsStr(other.toString)
      else JsObj(intoMap(other.asInstanceOf[AnyRef], methods, mapper))
  }
  private def intoMap(target: AnyRef, methods: List[MethodDef], mapper: PartialFunction[Any, Any]): Map[String, JsVal] = {
    methods.foldLeft(Map.empty[String, JsVal]) {
      case (map, method) => JsVal(method invoke target, mapper) match {
        case JsNull => map
        case value => map.updated(method.propName, value)
      }
    }
  }

  private class MethodDef(val propName: String, private val method: Method) {
    def this(method: Method) = this(method.getName, method)
    def this(method: Method, name: String) = this(name, method)
    def invoke(ref: AnyRef): Any = method invoke ref
    override def toString() = s"$propName = $method"
    override def hashCode = method.getName.hashCode ^ method.getReturnType.hashCode
    override def equals(any: Any): Boolean = any match {
      case that: MethodDef =>
        this.method.getName == that.method.getName &&
          this.method.getReturnType == that.method.getReturnType
      case _ => false
    }
  }
  private[this] val getters = new ClassValue[List[MethodDef]] {
    private[this] val excludedClasses = classOf[Object] :: classOf[Product] :: Nil
    private[this] lazy val excludeMethods = excludedClasses.flatMap(this.get).toSet

    def computeValue(cls: Class[_]) = {
      val isExcludedClass = excludedClasses contains cls
      val beanMethods: Set[MethodDef] = if (isExcludedClass) Set.empty
      else {
        Introspector.getBeanInfo(cls)
          .getPropertyDescriptors
          .flatMap { pd =>
            Option(pd.getReadMethod)
              .map(new MethodDef(_, pd.getName))
          }.filterNot(excludeMethods).toSet
      }

      val isBeanOrCC = beanMethods.nonEmpty || cls.getInterfaces.contains(classOf[Product])

      if (isBeanOrCC || isExcludedClass) {
        val methods = cls.getMethods
          .filter(_.getParameterCount == 0)
          .filter(_.getReturnType != Void.TYPE)
          .filterNot(_.getName contains "$")
          .filterNot(m => Modifier isStatic m.getModifiers)
          .map(new MethodDef(_)).filterNot(beanMethods) ++ beanMethods
        if (isExcludedClass) methods.toList // Methods from excluded classes themselves
        else methods.filterNot(excludeMethods).toList // Exclude methods from excluded classes
      } else Nil
    }
  }

  def parse(json: CharSequence, offset: Int = 0): JsVal = {
    val parser = new DefaultParser(json, offset)
    parser.parse()
  }

}

private final class DefaultParser(json: CharSequence, offset: Int)
extends AbstractParser(json, offset) {

  type JsVal = scuff.json.JsVal
  type JsBool = scuff.json.JsBool
  def True = scuff.json.JsBool.True
  def False = scuff.json.JsBool.False
  type JsObj = scuff.json.JsObj
  def JsObj(m: Map[String, JsVal])(implicit config: JsVal.Config): JsObj =
    if (m.isEmpty) scuff.json.JsObj.Empty
    else new scuff.json.JsObj(m)
  type JsArr = scuff.json.JsArr
  def JsArr(values: Seq[JsVal])(implicit config: JsVal.Config): JsArr =
    if (values.isEmpty) scuff.json.JsArr.Empty
    else new scuff.json.JsArr(values: _*)
  type JsStr = scuff.json.JsStr
  def JsStr(s: String): JsStr = new scuff.json.JsStr(s)
  type JsNull = scuff.json.JsNull.type
  def JsNull: JsNull = scuff.json.JsNull
  type JsNum = scuff.json.JsNum
  def JsNum(n: Number): JsNum = new scuff.json.JsNum(n)
  def Zero: JsNum = scuff.json.JsNum.Zero

}
