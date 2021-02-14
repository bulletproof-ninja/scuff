package scuff.json

import java.beans.Introspector
import java.io.File
import java.lang.reflect.{Method, Modifier, InvocationTargetException}
import java.math.{MathContext, BigDecimal => JBigDec, BigInteger => JBigInt}
import java.net._
import java.time.temporal.Temporal
import java.util.{UUID, Locale, IdentityHashMap, TimeZone}

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._
import scala.reflect.{ClassTag, classTag}

import scuff.{Base64, EmailAddress}

import language.dynamics

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

  private[json] def compressKeysInternal(
    dictionary: collection.concurrent.Map[Int, String],
    dictSnapshot: Map[String, Int],
    isConcurrent: Boolean)
    : (Map[String, Int], JsObj) = {

    @tailrec
    def addPropToDict(prop: String): Int = {
      val newOffset =
        if (dictionary.isEmpty) 0
        else dictionary.keysIterator.max + 1
      dictionary.putIfAbsent(newOffset, prop) match {
        case None =>
          newOffset
        case Some(existingProp) => // Race condition
          if (existingProp == prop) newOffset
          else addPropToDict(prop)
      }
    }

    var workIndex = dictSnapshot

    @tailrec
    def indexOf(prop: String, resetWorkIndexWhenNotFound: Boolean = isConcurrent): Int = {
      workIndex.getOrElse(prop, -1) match {
        case -1 =>
          if (resetWorkIndexWhenNotFound) {
            workIndex = JsObj.invert(dictionary)
            indexOf(prop, false)
          } else {
            val offset = addPropToDict(prop)
            workIndex = JsObj.invert(dictionary)
            offset
          }
        case offset => offset
      }
    }
    val compressed =
      props.foldLeft(Map.empty[String, JsVal]) {
        case (byIndex, (prop, obj: JsObj)) =>
          val (updIndex, compressedObj) =
            obj.compressKeysInternal(dictionary, workIndex, isConcurrent)
          workIndex = updIndex
          byIndex.updated(indexOf(prop).toString, compressedObj)
        case (byIndex, (prop, arr: JsArr)) =>
          val (updIndex, compressedArr) =
            arr.compressObjKeysInternal(dictionary, workIndex, isConcurrent)
          workIndex = updIndex
          byIndex.updated(indexOf(prop).toString, compressedArr)
        case (byIndex, (prop, value)) =>
          byIndex.updated(indexOf(prop).toString, value)
      }

    workIndex -> new JsObj(compressed)

  }

  /**
    * Compress the keys in this object,
    * using a dictionary.
    * @param dictionary The dictionary used and populated during compression
    * @param isConcurrentDictionary Flag indicating if dictionary is used concurrently.
    * If `true`, certain steps are taken to minimize duplicate properties, but at the cost of speed.
    * @param dictionarySnapshot Optional inverted dictionary snapshot.
    * Can improve performance if available.
    */
  def compressKeys(
      dictionary: collection.concurrent.Map[Int, String],
      isConcurrentDictionary: Boolean = false,
      dictionarySnapshot: Map[String, Int]= Map.empty): JsObj =
    compressKeysInternal(
        dictionary,
        if (dictionarySnapshot.nonEmpty) dictionarySnapshot else JsObj.invert(dictionary),
        isConcurrentDictionary)
      ._2

  def restoreCompressedKeys(
      dictionary: Array[String],
      isLenient: Boolean = true): JsObj = {

    def resolve(prop: String): String =
      try dictionary(prop.toInt) catch {
        case _: NumberFormatException =>
          if (isLenient) prop
          else  throw new IllegalStateException(
            s"""Found invalid offset value: "$prop", expected integer content. Enable leniency to use as-is.""")
        case _: IndexOutOfBoundsException =>
          throw new IllegalStateException(
            s"""Invalid dictionary index value of $prop is outside dictionary size of ${dictionary.length} """)
      }

    val restored =
      props.foldLeft(Map.empty[String, JsVal]) {
        case (byProp, (offset, obj: JsObj)) =>
          byProp.updated(resolve(offset), obj.restoreCompressedKeys(dictionary, isLenient))
        case (byProp, (offset, arr: JsArr)) =>
          byProp.updated(resolve(offset), arr.restoreCompressedObjKeys(dictionary, isLenient))
        case (byProp, (offset, value)) =>
          byProp.updated(resolve(offset), value)
      }
    new JsObj(restored)

  }

}
object JsObj {
  def Empty(implicit config: JsVal.Config) = JsObj()
  def apply(
      props: (String, JsVal)*)(
      implicit
      config: JsVal.Config): JsObj =
    new JsObj(props.toMap)

  private[json] def invert(
    index: collection.concurrent.Map[Int, String])
    : Map[String, Int] =
  index.map {
    case (offset, prop) => prop -> offset
  }.toMap

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

  private[json] def compressObjKeysInternal(
      dictionary: collection.concurrent.Map[Int, String],
      dictSnapshot: Map[String, Int],
      isConcurrent: Boolean): (Map[String, Int], JsArr) = {

    var workIndex = dictSnapshot

    val compressed = values.map {
        case obj: JsObj =>
          val (updIndex, compressed) = obj.compressKeysInternal(dictionary, workIndex, isConcurrent)
          workIndex = updIndex
          compressed
        case arr: JsArr =>
          val (updIndex, compressed) = arr.compressObjKeysInternal(dictionary, workIndex, isConcurrent)
          workIndex = updIndex
          compressed
        case other =>
          other
      }
    workIndex -> new JsArr(compressed: _*)
  }

  /**
    * Compress any JSON object keys in array, if they exist.
    * @param dictionary The dictionary used and populated during compression
    * @param isConcurrentDictionary Flag indicating if dictionary is used concurrently.
    * If `true`, certain steps are taken to minimize duplicate properties, but at the cost of speed.
    * @param dictionarySnapshot Optional inverted dictionary snapshot.
    * Can improve performance if available.
    */
  def compressObjKeys(
      dictionary: collection.concurrent.Map[Int, String],
      isConcurrentDictionary: Boolean = false,
      dictionarySnapshot: Map[String, Int] = Map.empty): JsArr =
    compressObjKeysInternal(
        dictionary,
        if (dictionarySnapshot.nonEmpty) dictionarySnapshot else JsObj.invert(dictionary),
        isConcurrentDictionary)
      ._2

  /**
    * Restore any JSON object keys,
    * which has been compressed.
    * @see compressObjKeys
    * @param dictionary The dictionary as an array. Will not be modified.
    * @param isLenient If `true` will assume that any non-integer key is a verbatim key. Defaults to `false`
    */
  def restoreCompressedObjKeys(
      dictionary: Array[String],
      isLenient: Boolean = false): JsArr = {
    val restored = values.map {
        case obj: JsObj => obj.restoreCompressedKeys(dictionary, isLenient)
        case arr: JsArr => arr.restoreCompressedObjKeys(dictionary, isLenient)
        case other => other
      }
    new JsArr(restored: _*)
  }

}
object JsArr {
  def Empty(implicit config: JsVal.Config) = JsArr()

  def fromMap(
    indexed: collection.Map[Int, String])(
    implicit
    config: JsVal.Config): JsArr =
  scuff.json.fromMap(indexed.mapValues(JsStr(_)))

}
final case class JsBool private (value: Boolean) extends JsVal {
  override def asBool = this
  def toJson(implicit config: JsVal.Config) = if (value) "true" else "false"
}
object JsBool {
  def apply(truh: Boolean): JsBool = if (truh) True else False
  val True = new JsBool(true)
  val False = new JsBool(false)
}

object JsVal {

  /**
   * @param escapeSlash Escape the character `/` as `\/` in strings?
   * @param upperCaseHex Output hexadecimal digits in upper case?
   * @param undefinedAccess Define behavior when accessing an undefined array index or object property.
   * The function parameter of `Any` is expected to be either an `Int` index value, or `String` property name.
   */
  case class Config(
      escapeSlash: Boolean = false,
      upperCaseHex: Boolean = false,
      undefinedAccess: Any => JsVal = _ => JsUndefined) {

    /**
      * @param onUndefined
      * The function parameter of `Any` is expected to be either
      * an `Int` index value, or `String` property name.
      */
    def withUndefinedAccess(onUndefined: Any => JsVal): Config =
      this.copy(undefinedAccess = onUndefined)

  }

  implicit val DefaultConfig = new Config()

  implicit def toJsVal(str: String): JsVal = if (str == null) JsNull else JsStr(str)
  implicit def toJsVal(num: Number): JsVal = if (num == null) JsNull else JsNum(num)
  implicit def toJsVal(num: Long): JsNum = JsNum(num: Number)
  implicit def toJsVal(num: Double): JsNum = JsNum(num: Number)
  implicit def toJsVal(b: Boolean): JsBool = JsBool(b)
  implicit def toJsVal(m: Map[String, Any]): JsVal = if (m == null) JsNull else JsObj(m.map(e => e._1 -> JsVal(e._2)))
  implicit def toJsVal(a: Iterable[Any]): JsVal = if (a == null) JsNull else JsArr(a.iterator.map(JsVal(_)).toSeq: _*)
  implicit def toJsVal(t: (String, Any)): (String, JsVal) = t._1 -> JsVal(t._2)

  implicit def stringValue(js: JsStr): String = js.value
  implicit def toByte(js: JsNum): Byte = js.toByte
  implicit def toShort(js: JsNum): Short = js.toShort
  implicit def toInt(js: JsNum): Int = js.toInt
  implicit def toLong(js: JsNum): Long = js.toLong
  implicit def toFloat(js: JsNum): Float = js.toFloat
  implicit def toDouble(js: JsNum): Double = js.toDouble
  implicit def toBigDecimal(js: JsNum): BigDecimal = js.toBigDec()
  implicit def toJavaBigDecimal(js: JsNum): JBigDec = js.toBigDec().underlying
  implicit def toBigInt(js: JsNum): BigInt = js.toBigInt
  implicit def toBigInteger(js: JsNum): JBigInt = js.toBigInteger
  implicit def boolValue(js: JsBool): Boolean = js.value

  def apply(any: Any, mapper: PartialFunction[Any, Any] = PartialFunction.empty): JsVal =
    try toJsVal(any, mapper) catch {
      case cause: CyclicReferenceDetected =>
        throw new RuntimeException(s"`${any.getClass.getName}` contains type that must be mapped explicitly: $any", cause)
    }

  private def toJsVal(
      any: Any,
      mapper: PartialFunction[Any, Any],
      seen: IdentityHashMap[AnyRef, AnyRef] = null)
      : JsVal =
    any match {
      case jv: JsVal => jv
      case any if mapper isDefinedAt any => JsVal(mapper(any), mapper)
      case null => JsNull
      case n: Number => JsNum(n)
      case s: CharSequence => JsStr(s.toString)
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
      case bytes: Array[Byte] => JsStr(Base64.RFC_4648.encode(bytes).toString)
      case i: Iterable[_] => JsArr(i.iterator.map(JsVal(_, mapper)).toSeq: _*)
      case m: java.util.Map[_, _] => JsObj {
        m.asScala.iterator.filterNot(_._2 == JsUndefined).map {
          case (key, value) => String.valueOf(key) -> JsVal(value, mapper)
        }.toMap
      }
      case i: java.lang.Iterable[_] => JsArr(i.iterator.asScala.map(JsVal(_, mapper)).toSeq: _*)
      case tz: TimeZone => JsStr(tz.getID)
      case ts: java.util.Date => JsNum(ts.getTime)
      case l: Locale => JsStr(l.toLanguageTag)
      case asString @ (_: URL | _: URI | _: UUID | _: Temporal | _: EmailAddress | _: File) => JsStr(asString.toString)
      case other =>
        val methods = other match {
          case _: AnyRef => getters.get(other.getClass)
          case _ => Nil
        }
        if (methods.isEmpty) new JsStr(other.toString)
        else JsObj(intoMap(other.asInstanceOf[AnyRef], methods, mapper, seen))
    }

  case class CyclicReferenceDetected(cls: Class[_])
  extends RuntimeException(
    s"Class too complex to convert automatically: ${cls.getName}")

  private def intoMap(
      target: AnyRef,
      methods: List[MethodDef],
      mapper: PartialFunction[Any, Any],
      seenOrNull: IdentityHashMap[AnyRef, AnyRef]): Map[String, JsVal] = try {
    val seen = if (seenOrNull ne null) seenOrNull else new IdentityHashMap[AnyRef, AnyRef]
    if (seen.put(target, target) eq target) throw CyclicReferenceDetected(target.getClass)
    else methods.foldLeft(Map.empty[String, JsVal]) {
      case (map, method) =>
        toJsVal(method invoke target, mapper, seen) match {
          case JsNull => map
          case value => map.updated(method.propName, value)
        }
    }
  } catch {
    case _: StackOverflowError => throw CyclicReferenceDetected(target.getClass)
  }

  private class MethodDef(val propName: String, private val method: Method) {
    def this(method: Method) = this(method.getName, method)
    def this(method: Method, name: String) = this(name, method)
    def invoke(ref: AnyRef): Any = try method invoke ref catch {
      case e: InvocationTargetException => throw e.getCause
    }
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
