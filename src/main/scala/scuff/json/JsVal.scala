package scuff.json

import java.lang.reflect.Modifier
import java.lang.reflect.Method

import collection.JavaConverters._
import java.math.BigDecimal
import java.math.MathContext
import java.util.regex.Pattern
import java.util.regex.Matcher
import scala.reflect.{ ClassTag, classTag }
import language.{ existentials, dynamics }

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
final case class JsNum(value: Number) extends JsVal {
  override def asNum = this
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
    case bd: java.math.BigDecimal => BigInt(bd.toBigInteger)
    case bi: java.math.BigInteger => BigInt(bi)
    case bd: scala.math.BigDecimal => bd.toBigInt
    case bi: BigInt => bi
    case num => BigInt(num.toString)
  }
  def toBigDec(): scala.BigDecimal = toBigDec(null)
  def toBigDec(mc: MathContext): scala.BigDecimal = value match {
    case bd: java.math.BigDecimal =>
      if (mc == null) scala.BigDecimal(bd)
      else new scala.math.BigDecimal(bd, mc)
    case bi: java.math.BigInteger =>
      if (mc == null) scala.BigDecimal(new BigDecimal(bi))
      else new scala.math.BigDecimal(new BigDecimal(bi, mc), mc)
    case bd: scala.math.BigDecimal =>
      if (mc == null || mc == bd.mc) bd
      else bd(mc)
    case bi: BigInt =>
      if (mc == null) scala.math.BigDecimal(bi)
      else scala.math.BigDecimal(bi, mc)
    case num =>
      if (mc == null) scala.math.BigDecimal(num.toString)
      else scala.math.BigDecimal(num.toString, mc)
  }
}

object JsNum {
  val NaN = JsNum(Double.NaN)
  val PositiveInfinity = JsNum(Double.PositiveInfinity)
  val NegativeInfinity = JsNum(Double.NegativeInfinity)
  val Zero = JsNum(BigDecimal.ZERO)
  val One = JsNum(BigDecimal.ONE)
}

final case class JsStr(value: String) extends JsVal {
  override def asStr = this
  override def asNum = value match {
    case "NaN" => JsNum.NaN
    case "Infinity" => JsNum.PositiveInfinity
    case "-Infinity" => JsNum.NegativeInfinity
    case _ => super.asNum
  }
  def toJson(implicit config: JsVal.Config) =
    if (value == null) JsNull.toJson
    else s""""${JsStr.escape(value, config.escapeSlashInStrings)}""""
}
object JsStr {
  private[this] val (escapesInclSlash, escapesExclSlash) = {
    val escapeBackslash = Pattern.compile("\\", Pattern.LITERAL) -> Matcher.quoteReplacement("\\\\")
    val escapeSlash = Pattern.compile("/", Pattern.LITERAL) -> Matcher.quoteReplacement("\\/")
    val escapeOthers = List(
      Pattern.compile("\"", Pattern.LITERAL) -> Matcher.quoteReplacement("\\\""),
      Pattern.compile("\b", Pattern.LITERAL) -> Matcher.quoteReplacement("\\b"),
      Pattern.compile("\f", Pattern.LITERAL) -> Matcher.quoteReplacement("\\f"),
      Pattern.compile("\n", Pattern.LITERAL) -> Matcher.quoteReplacement("\\n"),
      Pattern.compile("\r", Pattern.LITERAL) -> Matcher.quoteReplacement("\\r"),
      Pattern.compile("\t", Pattern.LITERAL) -> Matcher.quoteReplacement("\\t"))

    // escapeBackslash must always be first

    val inclSlash = escapeBackslash :: escapeSlash :: escapeOthers
    val exclSlash = escapeBackslash :: escapeOthers

    inclSlash -> exclSlash
  }
  private[json] def escape(str: String, escapeSlash: Boolean): String = {
    val escapes = if (escapeSlash) escapesInclSlash else escapesExclSlash
    escapes.foldLeft(str) {
      case (str, (m, r)) => m.matcher(str).replaceAll(r)
    }
  }
}

final case object JsNull extends JsVal {
  def toJson(implicit config: JsVal.Config) = "null"
}
final case object JsUndefined extends JsVal {
  def toJson(implicit config: JsVal.Config) = "null" // Not supposed to be serialized, but probably better to emit "null" than fail
}
final case class JsObj(props: Map[String, JsVal]) extends JsVal
  with Iterable[(String, JsVal)]
  with Dynamic {

  def this(props: (String, JsVal)*) = this(props.toMap)
  override def asObj = this
  def toJson(implicit config: JsVal.Config) =
    if (props == null) JsNull.toJson
    else props.iterator.map {
      case (name, value) => s""""${JsStr.escape(name, config.escapeSlashInStrings)}":${value.toJson}"""
    }.mkString("{", ",", "}")
  def get(name: String): Option[JsVal] = if (props != null) props.get(name) else None
  def apply(name: String): JsVal = if (props != null) props.getOrElse(name, JsUndefined) else JsUndefined
  def selectDynamic(name: String): JsVal = apply(name)
  def iterator = if (props != null) props.iterator else Iterator.empty
}
object JsObj {
  def apply(props: (String, JsVal)*): JsObj = new JsObj(props.toMap)
}
final case class JsArr(values: JsVal*) extends JsVal with Iterable[JsVal] {
  override def asArr = this
  def toJson(implicit config: JsVal.Config) = values.iterator.map(_.toJson).mkString("[", ",", "]")
  def get(idx: Int): Option[JsVal] = if (idx >= 0 && idx < values.size) Some(values(idx)) else None
  def apply(idx: Int): JsVal = if (idx >= 0 && idx < values.size) values(idx) else JsUndefined
  def iterator = values.iterator
  def length = values.size
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
  import language.implicitConversions

  /**
   * @param escapeSlashInStrings Escape the character `/` as `\/` in strings?
   */
  case class Config(escapeSlashInStrings: Boolean)

  implicit val defaultConfig = Config(escapeSlashInStrings = false)

  implicit def toJsVal(str: String): JsVal = if (str == null) JsNull else JsStr(str)
  implicit def toJsVal(num: java.lang.Number): JsVal = if (num == null) JsNull else JsNum(num)
  implicit def toJsVal(num: Long): JsVal = JsNum(num)
  implicit def toJsVal(num: Double): JsVal = JsNum(num)
  implicit def toJsVal(num: Float): JsVal = JsNum(num)
  implicit def toJsVal(b: Boolean): JsVal = if (b) JsBool.True else JsBool.False
  implicit def toJsVal(m: Map[String, Any]): JsVal = if (m == null) JsNull else JsObj(m.mapValues(JsVal(_)).toMap)
  implicit def toJsVal(a: Iterable[Any]): JsVal = if (a == null) JsNull else JsArr(a.iterator.map(JsVal(_)).toSeq: _*)
  implicit def toJsVal(t: (String, Any)): (String, JsVal) = t._1 -> JsVal(t._2)

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
    case cc: (AnyRef with Product) => JsObj(intoMap(cc, mapper))
    case m: java.util.Map[_, _] => JsObj {
      m.asScala.iterator.filterNot(_._2 == JsUndefined).map {
        case (key, value) => String.valueOf(key) -> JsVal(value, mapper)
      }.toMap
    }
    case i: java.lang.Iterable[_] => JsArr(i.iterator.asScala.map(JsVal(_, mapper)).toSeq: _*)
    case other => JsStr(String valueOf other)
  }
  private def intoMap(cc: AnyRef with Product, mapper: PartialFunction[Any, Any]): Map[String, JsVal] = {
    val methods = getters.get(cc.getClass)
    methods.foldLeft(Map.empty[String, JsVal]) {
      case (map, method) => JsVal(method invoke cc, mapper) match {
        case JsNull => map
        case value => map.updated(method.name, value)
      }
    }
  }

  private case class MethodDef(returnType: Class[_], name: String)(method: Method) {
    def this(method: Method) = this(method.getReturnType, method.getName)(method)
    def invoke(ref: AnyRef): Any = method invoke ref
  }
  private[this] val getters = new ClassValue[List[MethodDef]] {
    private[this] val excludeClasses = List(classOf[Object], classOf[Product])
    private[this] val excludeMethods = excludeClasses.flatMap(this.get).toSet

    def computeValue(cls: Class[_]) = {
      val methods = cls.getMethods
        .filter(_.getParameterCount == 0)
        .filter(_.getReturnType != Void.TYPE)
        .filterNot(_.getName contains "$")
        .filterNot(m => Modifier isStatic m.getModifiers)
        .map(new MethodDef(_))
      if (excludeClasses contains cls) methods.toList
      else methods.toList.filterNot(excludeMethods)
    }
  }

  def parse(json: CharSequence, offset: Int = 0): JsVal = {
    val parser = new Parser(json, offset)
    parser.parse()
  }

}

private class Parser(
    json: CharSequence,
    offset: Int) {

  private[this] var chars = new Array[Char](64)
  private[this] var _charsIdx = 0
  private def charsIdx_=(value: Int): Unit = {
    if (value >= chars.length) {
      val oldChars = chars
      chars = new Array[Char](oldChars.length * 2)
      System.arraycopy(oldChars, 0, chars, 0, oldChars.length)
    }
    _charsIdx = value
  }
  private def charsIdx: Int = _charsIdx

  private[this] var pos = offset

  def parse(): JsVal = try parseAny() catch {
    case ioob: IndexOutOfBoundsException =>
      throw new MalformedJSON(s"Incomplete JSON", ioob)
  } finally {
    while (pos < json.length) json.charAt(pos) match {
      case ' ' | '\t' | '\r' | '\n' => pos += 1
      case unexpected => throwUnexpectedCharException(unexpected)
    }
  }

  private def parseAny(): JsVal = {
    json.charAt(pos) match {
      case ' ' | '\t' | '\r' | '\n' =>
        pos += 1; parseAny()
      case '"' =>
        pos += 1; JsStr(parseString())
      case '{' =>
        pos += 1; parseObject()
      case '[' =>
        pos += 1; parseArray()
      case 'n' =>
        pos += 1; parseLiteral("null"); JsNull
      case 't' =>
        pos += 1; parseLiteral("true"); JsBool.True
      case 'f' =>
        pos += 1; parseLiteral("false"); JsBool.False
      case ch @ (',' | '}' | ']') => throwUnexpectedCharException(ch)
      case '0' =>
        if (pos + 1 < json.length) {
          json.charAt(pos + 1) match {
            case ',' | '}' | ']' | ' ' | '\t' | '\r' | '\n' =>
              pos += 1; JsNum.Zero
            case '.' => parseNumber()
            case _ => throw new MalformedJSON(s"Numbers cannot start with 0, offset $pos")
          }
        } else {
          pos += 1
          JsNum.Zero
        }
      case _ => parseNumber()
    }
  }

  private def parseLiteral(literal: String): Unit = {
    assert(literal(0) == json.charAt(pos - 1))
    var idx = 1
    while (idx < literal.length) {
      if (literal.charAt(idx) == json.charAt(pos)) {
        pos += 1
        idx += 1
      } else {
        val found = json.subSequence(pos - idx, pos)
        throw new MalformedJSON(s"Expected `$literal`, found `$found` at offset $pos")
      }
    }
  }

  private def parseNumber(): JsNum = {

      def toJsNum(): JsNum = {
        val num: Number = try new BigDecimal(chars, 0, charsIdx) catch {
          case _: NumberFormatException =>
            try java.lang.Double.parseDouble(new String(chars, 0, charsIdx)) catch {
              case nfe: NumberFormatException =>
                throw new MalformedJSON(s"Invalid number at offset $pos", nfe)
            }
        }
        charsIdx = 0
        JsNum(num)
      }

    if (pos < json.length) json.charAt(pos) match {
      case ',' | '}' | ']' | ' ' | '\t' | '\r' | '\n' =>
        toJsNum()
      case n =>
        chars(charsIdx) = n
        charsIdx += 1
        pos += 1
        parseNumber()
    }
    else toJsNum()
  }

  private def parseString(): String = {
    assert(json.charAt(pos - 1) == '"')

      def hexToDec(pos: Int): Int = json.charAt(pos) match {
        case d @ ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') => d - '0'
        case h @ ('a' | 'b' | 'c' | 'd' | 'e' | 'f') => h - 87
        case h @ ('A' | 'B' | 'C' | 'D' | 'E' | 'F') => h - 55
        case unexpected => throwUnexpectedCharException(unexpected, pos)
      }

      def parseString(pos: Int): Int = json.charAt(pos) match {
        case '\\' =>
          json.charAt(pos + 1) match {
            case literal @ ('"' | '\\' | '/') =>
              chars(charsIdx) = literal
              charsIdx += 1
              parseString(pos + 2)
            case 'b' =>
              chars(charsIdx) = '\b'
              charsIdx += 1
              parseString(pos + 2)
            case 'f' =>
              chars(charsIdx) = '\f'
              charsIdx += 1
              parseString(pos + 2)
            case 'n' =>
              chars(charsIdx) = '\n'
              charsIdx += 1
              parseString(pos + 2)
            case 'r' =>
              chars(charsIdx) = '\r'
              charsIdx += 1
              parseString(pos + 2)
            case 't' =>
              chars(charsIdx) = '\t'
              charsIdx += 1
              parseString(pos + 2)
            case 'u' =>
              chars(charsIdx) = {
                hexToDec(pos + 2) << (4 * 3) |
                  hexToDec(pos + 3) << (4 * 2) |
                  hexToDec(pos + 4) << 4 |
                  hexToDec(pos + 5)
              }.asInstanceOf[Char]
              charsIdx += 1
              parseString(pos + 6)
            case unexpected => throwUnexpectedCharException(unexpected, pos + 1)
          }
        case '"' => pos + 1
        case invalid @ ('\b' | '\f' | '\n' | '\r' | '\t') =>
          val asHex = invalid.toHexString
          throw new MalformedJSON(s"Unescaped control character 0x0$asHex found at offset $pos")
        case ch =>
          chars(charsIdx) = ch
          charsIdx += 1
          parseString(pos + 1)
      }

    pos = parseString(pos)
    val string = new String(chars, 0, charsIdx)
    charsIdx = 0
    string
  }

  private def hasMore(CloseChar: Char, isEmpty: Boolean): Boolean = {
    json.charAt(pos) match {
      case ',' =>
        if (isEmpty) throwUnexpectedCharException(',')
        pos += 1; true
      case CloseChar =>
        pos += 1; false
      case ' ' | '\t' | '\r' | '\n' =>
        pos += 1; hasMore(CloseChar, isEmpty)
      case _ => true
    }
  }

  private def throwUnexpectedCharException(unexpected: Char, pos: Int = this.pos) =
    throw new MalformedJSON(s"Unexpected character `$unexpected`, offset $pos")

  private def parseObject(): JsObj = {
    assert(json.charAt(pos - 1) == '{')

      def forwardPastColon(): Unit = {
        json.charAt(pos) match {
          case ':' => pos += 1 // Done
          case ' ' | '\t' | '\r' | '\n' =>
            pos += 1; forwardPastColon()
          case unexpected => throwUnexpectedCharException(unexpected)
        }
      }

      def parseProperty(map: Map[String, JsVal]): Map[String, JsVal] = {
        if (hasMore('}', map.isEmpty)) json.charAt(pos) match {
          case '"' =>
            pos += 1
            val name = parseString()
            forwardPastColon()
            val value = parseAny()
            parseProperty(map.updated(name, value))

          case ' ' | '\t' | '\r' | '\n' =>
            pos += 1; parseProperty(map)
          case unexpected => throwUnexpectedCharException(unexpected)
        }
        else map
      }

    JsObj(parseProperty(Map.empty))
  }

  private def parseArray(): JsArr = {
    assert(json.charAt(pos - 1) == '[')

      def parseArray(seq: Vector[JsVal]): Vector[JsVal] = {
        if (hasMore(']', seq.isEmpty)) {
          parseArray(seq :+ parseAny())
        } else seq
      }

    JsArr(parseArray(Vector.empty): _*)
  }

}
