package scuff

import java.util.regex.Pattern

object Base64 {

  /** Wrap existing `Array[Byte]` codec in RFC 4648 URL/filename safe Base64 codec. */
  def apply[A](codec: Codec[A, Array[Byte]]) = new Codec[A, CharSequence] {
    def encode(a: A): CharSequence = RFC_4648 encode (codec encode a)
    def decode(b: CharSequence): A = codec decode (RFC_4648 decode b)
  }

  /**
   * If the base64 encoding contains end-of-line
   * chars, use this to remove prior to decoding.
   */
  def removeEOLs(base64: CharSequence, lineLength: Int = 0): CharSequence = {
      @inline
      def isCRLF(c: Char): Boolean = c == '\r' || c == '\n'
      def trailingCRLF(chars: Array[Char], until: Int = 0): Int = {
        val stopCount = chars.length - until
        var count = 1
        while (count <= stopCount && isCRLF(chars(chars.length - count))) count += 1
        count - 1
      }
      @annotation.tailrec
      def removeLineFeed(chars: Array[Char], lineCount: Int, newLineLen: Int, line: Int = 1): Int = {
        val toIdx = lineLength * line
        val fromIdx = (lineLength + newLineLen) * line
        if (line + 1 == lineCount) {
          val trailing = trailingCRLF(chars, fromIdx)
          val length = chars.length - fromIdx - trailing
          System.arraycopy(chars, fromIdx, chars, toIdx, length)
          line * newLineLen + trailing
        } else {
          System.arraycopy(chars, fromIdx, chars, toIdx, lineLength)
          removeLineFeed(chars, lineCount, newLineLen, line + 1)
        }
      }
      @annotation.tailrec
      def toCharArray(cs: CharSequence, chars: Array[Char], offset: Int = 0): Array[Char] = {
        if (offset == chars.length) {
          chars
        } else {
          chars(offset) = cs.charAt(offset)
          toCharArray(cs, chars, offset + 1)
        }
      }
    if (lineLength > 0) {
      val chars = base64 match {
        case str: String => str.toCharArray()
        case cs: unsafe.CharSeq => cs.toCharArray()
        case _ => toCharArray(base64, new Array[Char](base64.length))
      }
      val newLineLen =
        if (chars.length >= lineLength+2) {
          if (isCRLF(chars(lineLength+1))) 2 else 1
        } else {
          1
        }

      val lineCount = chars.length / (lineLength + newLineLen) + math.signum(chars.length % (lineLength + newLineLen))
      val removed = if (lineCount == 1) trailingCRLF(chars) else removeLineFeed(chars, lineCount, newLineLen)
      new unsafe.CharSeq(chars, 0, chars.length - removed)
    } else {
      EOLRemover.matcher(base64).replaceAll("")
    }
  }

  private[this] final val LastSixBits = 63
  private[this] val RFC4648BaseChars = {
    ('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9') :+ '-' :+ '_'
  }.toArray
  private[this] val RFC4648BaseCharIndex = toIndexByChar(RFC4648BaseChars)
  @annotation.tailrec
  private def toIndexByChar(base: Array[Char], index: Int = 0, charIndex: Array[Byte] = new Array(128)): Array[Byte] = {
    if (index > LastSixBits) {
      charIndex
    } else {
      val ch = base(index)
      if (ch >= charIndex.length) throw new IllegalArgumentException(s"Base char is out of range: $ch (0x${Integer.toHexString(ch)})")
      charIndex(ch) = index.asInstanceOf[Byte]
      toIndexByChar(base, index + 1, charIndex)
    }
  }
  private def newSplitter(len: Int): Pattern = s"""(?s).{1,$len}""".r.pattern

  type Base64 = Codec[Array[Byte], CharSequence]

  /** Define custom base64 codec. */
  def Custom(char62: Char, char63: Char, withPadding: Boolean = true, paddingChar: Char = '=', maxLineLength: Int = 0): Base64 = {
    val baseChars = {
      val base = RFC4648BaseChars.clone
      base(62) = char62
      base(63) = char63
      base
    }
    val charIndex = toIndexByChar(baseChars, 62, RFC4648BaseCharIndex.clone)
    val splitter = if (maxLineLength > 0) {
      Some(newSplitter(maxLineLength) -> maxLineLength)
    } else None
    new Impl(baseChars, paddingChar, withPadding, charIndex, splitter)
  }

  private[this] val EOLRemover = """[\r\n]+""".r.pattern

  /** URL and filename safe encoding, no padding, no line breaks. */
  val RFC_4648: Base64 = new Impl(RFC4648BaseChars, '=', withPadding = false, RFC4648BaseCharIndex, None)
  def RFC_4648(withPadding: Boolean): Base64 = new Impl(RFC4648BaseChars, '=', withPadding, RFC4648BaseCharIndex, None)

  /** Default base-64 encoding, with padding and line breaks for every 76 characters. */
  val RFC_1521: Base64 = Custom('+', '/', withPadding = true, paddingChar = '=', 76)
  /** Default base-64 encoding, with padding and line breaks optional. */
  def RFC_1521(lineBreaks: Boolean): Base64 = Custom('+', '/', withPadding = true, paddingChar = '=', if (lineBreaks) 76 else 0)
  /** Default base-64 encoding, with padding and line breaks for every 76 characters. */
  def RFC_2045: Base64 = RFC_1521
  /** Default base-64 encoding, with padding and line breaks optional. */
  def RFC_2045(lineBreaks: Boolean): Base64 = RFC_1521(lineBreaks)

  private class Impl(baseChars: Array[Char], paddingChar: Char, withPadding: Boolean, charIdx: Array[Byte], lineSplitter: Option[(Pattern, Int)]) extends Base64 {

    private def encodeBytes(b1: Byte, b2: Byte, b3: Byte, chars: Array[Char], charOffset: Int, padding: Int) {
      import Numbers.unsigned
      val bits = unsigned(b1) << 16 | unsigned(b2) << 8 | unsigned(b3)
      padding match {
        case 0 =>
          chars(charOffset + 3) = baseChars(bits & LastSixBits)
          chars(charOffset + 2) = baseChars(bits >> 6 & LastSixBits)
        case 1 =>
          chars(charOffset + 3) = paddingChar
          chars(charOffset + 2) = baseChars(bits >> 6 & LastSixBits)
        case 2 =>
          chars(charOffset + 3) = paddingChar
          chars(charOffset + 2) = paddingChar
      }
      chars(charOffset + 1) = baseChars(bits >> 12 & LastSixBits)
      chars(charOffset) = baseChars(bits >> 18 & LastSixBits)
    }
    @annotation.tailrec
    private def encodeChunk(bytes: Array[Byte], chars: Array[Char], byteOffset: Int = 0, charOffset: Int = 0) {
      (bytes.length - byteOffset) match {
        case 0 => // Done
        case 1 =>
          encodeBytes(bytes(byteOffset), 0, 0, chars, charOffset, 2)
        case 2 =>
          encodeBytes(bytes(byteOffset), bytes(byteOffset + 1), 0, chars, charOffset, 1)
        case _ =>
          encodeBytes(bytes(byteOffset), bytes(byteOffset + 1), bytes(byteOffset + 2), chars, charOffset, 0)
          encodeChunk(bytes, chars, byteOffset + 3, charOffset + 4)
      }
    }

    private def decodeChars(c1: Char, c2: Char, c3: Char, c4: Char, bytes: Array[Byte], byteOffset: Int, padding: Int) {
      padding match {
        case 0 =>
          val bits = charIdx(c1) << 18 | charIdx(c2) << 12 | charIdx(c3) << 6 | charIdx(c4)
          bytes(byteOffset + 2) = (bits & 0xFF).asInstanceOf[Byte]
          bytes(byteOffset + 1) = (bits >> 8 & 0xFF).asInstanceOf[Byte]
          bytes(byteOffset) = (bits >> 16 & 0xFF).asInstanceOf[Byte]
        case 1 =>
          val bits = charIdx(c1) << 18 | charIdx(c2) << 12 | charIdx(c3) << 6
          bytes(byteOffset + 1) = (bits >> 8 & 0xFF).asInstanceOf[Byte]
          bytes(byteOffset) = (bits >> 16 & 0xFF).asInstanceOf[Byte]
        case 2 =>
          val bits = charIdx(c1) << 18 | charIdx(c2) << 12
          bytes(byteOffset) = (bits >> 16 & 0xFF).asInstanceOf[Byte]
      }
    }
    @annotation.tailrec
    private def decodeChunk(s: CharSequence, len: Int, bytes: Array[Byte], strOffset: Int = 0, byteOffset: Int = 0) {
      (len - strOffset) match {
        case 0 => // Done
        case 2 =>
          decodeChars(s.charAt(strOffset), s.charAt(strOffset + 1), 0, 0, bytes, byteOffset, 2)
        case 3 =>
          decodeChars(s.charAt(strOffset), s.charAt(strOffset + 1), s.charAt(strOffset + 2), 0, bytes, byteOffset, 1)
        case _ =>
          decodeChars(s.charAt(strOffset), s.charAt(strOffset + 1), s.charAt(strOffset + 2), s.charAt(strOffset + 3), bytes, byteOffset, 0)
          decodeChunk(s, len, bytes, strOffset + 4, byteOffset + 3)
      }
    }
    private def finishEncoding(chars: Array[Char], removePadding: Int, lineBreak: String = "\r\n"): CharSequence = {
      lineSplitter match {
        case None => new unsafe.CharSeq(chars, 0, chars.length - removePadding)
        case Some((lineSplitter, maxLineLen)) =>
          val maxLineCount = chars.length / maxLineLen + 1
          val sb = new java.lang.StringBuilder(maxLineCount * (maxLineLen + lineBreak.length))
          val m = lineSplitter.matcher(new unsafe.CharSeq(chars, 0, chars.length - removePadding))
          while (m.find) sb append m.group(0) append lineBreak
          sb.toString
      }

    }

    def encode(bytes: Array[Byte]): CharSequence =
      if (bytes.length == 0) "" else {
        val padding = bytes.length % 3 match {
          case 0 => 0
          case 1 => 2
          case 2 => 1
        }
        val chunkCount = (bytes.length + padding) / 3
        val chars = new Array[Char](chunkCount * 4)
        encodeChunk(bytes, chars)
        finishEncoding(chars, if (withPadding) 0 else padding)
      }
    private def invalidLength(s: CharSequence) = new IllegalArgumentException(s"Cannot decode string, invalid length: ${s.length}")
    def decode(s: CharSequence): Array[Byte] = {
      s.length match {
        case 0 => Array.empty
        case 1 => if (s.charAt(0) == paddingChar) Array.empty else throw invalidLength(s)
        case _ =>
          val observedPadding =
            if (s.charAt(s.length - 2) == paddingChar) {
              2
            } else if (s.charAt(s.length - 1) == paddingChar) {
              1
            } else {
              0
            }
          val dataLen = s.length - observedPadding
          val bytesLen = dataLen % 4 match {
            case 0 => (dataLen / 4) * 3
            case 1 => throw invalidLength(s)
            case 2 => (dataLen / 4) * 3 + 1
            case 3 => (dataLen / 4) * 3 + 2
          }
          val bytes = new Array[Byte](bytesLen)
          decodeChunk(s, dataLen, bytes)
          bytes
      }
    }
  }

}
