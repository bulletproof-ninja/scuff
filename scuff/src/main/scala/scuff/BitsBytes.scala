package scuff

import collection.generic.Growable

object BitsBytes {
  def unsigned(n: Int) = n & 0xFFFFFFFFL
  def unsigned(n: Short) = n & 0xFFFF
  def unsigned(n: Byte) = n & 0xFF
  private[this] val longUnsigner = (2 * BigInt(Long.MaxValue) + 2).underlying()
  def unsigned(value: Long) = {
    if (value < 0)
      new BigInt(java.math.BigInteger.valueOf(value) add longUnsigner)
    else
      BigInt(value)
  }

  @annotation.tailrec
  def hexEncode[T <: Growable[Char] with CharSequence](bytes: Array[Byte], g: T = new StringBuilder, offset: Int = 0): T = {
    if (offset < bytes.length) {
      val uint32 = unsigned(bytes(offset))
      if (uint32 < 16) g += '0'
      g ++= java.lang.Integer.toHexString(uint32)
      hexEncode(bytes, g, offset + 1)
    } else {
      g
    }
  }
}