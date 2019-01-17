package scuff.unsafe

import java.util.Arrays

/**
 * Unsafe `CharSequence` implementation, taking an
 * existing `Array[Char]` without copying and without
 * bounds checking. In other words, it assumes that
 * the user will only use indexing between `offset`
 * and `length`.
 */
final class CharSeq(chars: Array[Char], offset: Int = 0, len: Int = -1) extends CharSequence {
  def toCharArray(): Array[Char] =
    if (offset == 0 && length == chars.length) chars
    else Arrays.copyOfRange(chars, offset, offset + length)
  def charAt(idx: Int) = chars(offset + idx)
  def length() = if (len == -1) chars.length - offset else len
  def subSequence(strIdx: Int, endIdx: Int) = new CharSeq(chars, offset + strIdx, endIdx - strIdx)
  override def toString() = new String(chars, offset, length)
}
