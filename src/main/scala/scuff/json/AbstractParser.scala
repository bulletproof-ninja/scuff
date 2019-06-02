package scuff.json

abstract class AbstractParser(
    json: CharSequence,
    offset: Int) {

  type JsVal
  type JsBool <: JsVal
  def True: JsBool
  def False: JsBool
  type JsObj <: JsVal
  def JsObj(m: Map[String, JsVal]): JsObj
  type JsArr <: JsVal
  def JsArr(values: Seq[JsVal]): JsArr
  type JsStr <: JsVal
  def JsStr(s: String): JsStr
  type JsNull <: JsVal
  def JsNull: JsNull
  type JsNum <: JsVal
  def JsNum(n: Number): JsNum
  def Zero: JsNum

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
  @inline private def charsIdx: Int = _charsIdx

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
      case ' ' | '\t' | '\r' | '\n' | '\f' =>
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
        pos += 1; parseLiteral("true"); True
      case 'f' =>
        pos += 1; parseLiteral("false"); False
      case '-' =>
        chars(charsIdx) = '-'
        charsIdx += 1
        pos += 1;
        parseNumber(-1)
      case '0' =>
        if (pos + 1 < json.length) {
          json.charAt(pos + 1) match {
            case ',' | '}' | ']' | ' ' | '\t' | '\r' | '\n' =>
              pos += 1; Zero
            case '.' => parseNumber(0)
            case ch if ch >= '0' && ch <= '9' => throw new MalformedJSON(s"Numbers cannot start with 0, offset $pos")
            case unexpected => throwUnexpectedCharException(unexpected, pos + 1)
          }
        } else {
          pos += 1
          Zero
        }
      case ch if ch > '0' && ch <= '9' =>
        parseNumber(1)
      case ch => throwUnexpectedCharException(ch)
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

  private def parseNumber(sign: Byte, integer: Long = 0): JsNum = {

      def toJsNum(): JsNum = {
        val num: Number = if (sign != 0 && integer > 0) {
          assert(sign == -1 || sign == 1)
          sign * integer
        } else {
          try BigDecimal(new java.math.BigDecimal(chars, 0, charsIdx)) catch {
            case _: NumberFormatException =>
              try java.lang.Double.parseDouble(new String(chars, 0, charsIdx)) catch {
                case nfe: NumberFormatException =>
                  throw new MalformedJSON(s"Invalid number at offset $pos", nfe)
              }
          }
        }
        charsIdx = 0
        JsNum(num)
      }

    if (pos < json.length) json.charAt(pos) match {
      case ',' | '}' | ']' | ' ' | '\t' | '\r' | '\n' =>
        toJsNum()
      case n =>
        val isDigit = n >= '0' && n <= '9'
        chars(charsIdx) = n
        charsIdx += 1
        pos += 1
        parseNumber(if (isDigit && integer >= 0) sign else 0, (integer * 10) + (n - '0'))
    }
    else toJsNum()
  }

  private def parseString(): String = {
    assert(json.charAt(pos - 1) == '"')

      def hexToDec(pos: Int): Int = {
        val ch = json charAt pos
        if (ch >= '0' && ch <= '9') ch - '0'
        else if (ch >= 'a' && ch <= 'f') ch - 87
        else if (ch >= 'A' && ch <= 'F') ch - 55
        else throwUnexpectedCharException(ch, pos)
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

    JsArr(parseArray(Vector.empty))
  }

}
