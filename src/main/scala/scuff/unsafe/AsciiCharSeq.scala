package scuff.unsafe

import scuff.Numbers.unsigned

/**
 * Unsafe `CharSequence` implementation, taking an
 * existing `Array[Byte]` without copying and without
 * bounds checking and with the assumption that all chars
 * falls within the ASCII range of 0-255.
 * This also  assumes that the user will only use indexing
 * between 0 and `length`.
 */
final class AsciiCharSeq(ascii: Array[Byte], offset: Int = 0, len: Int = -1) extends CharSequence {
  def charAt(idx: Int) = unsigned(ascii(offset + idx)).asInstanceOf[Char]
  def length() = if (len == -1) ascii.length - offset else len
  def subSequence(strIdx: Int, endIdx: Int) = new AsciiCharSeq(ascii, offset + strIdx, endIdx - strIdx)
  override def toString() = new String(ascii, offset, length, AsciiCharSeq.ASCII)
}

private object AsciiCharSeq {
  val ASCII = java.nio.charset.Charset.forName("US-ASCII")
}
