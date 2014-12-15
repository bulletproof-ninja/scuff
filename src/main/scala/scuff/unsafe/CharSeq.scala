package scuff.unsafe

/**
 * Unsafe `CharSequence` implementation, taking an
 * existing `Array[Char]` without copying and without
 * bounds checking.
 */
final class CharSeq(chars: Array[Char], offset: Int = 0, len: Int = -1) extends CharSequence {
  def charAt(idx: Int) = chars(offset + idx)
  def length() = if (len == -1) chars.length - offset else len
  def subSequence(strIdx: Int, endIdx: Int) = new CharSeq(chars, offset + strIdx, endIdx - strIdx)
  override def toString() = new String(chars, offset, length)
}
