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

  def bytesToLong(arr: Array[Byte], offset: Int = 0): Long =
    (arr(offset): Long) << 56 |
      ((arr(offset + 1): Long) & 0xff) << 48 |
      ((arr(offset + 2): Long) & 0xff) << 40 |
      ((arr(offset + 3): Long) & 0xff) << 32 |
      ((arr(offset + 4): Long) & 0xff) << 24 |
      ((arr(offset + 5): Long) & 0xff) << 16 |
      ((arr(offset + 6): Long) & 0xff) << 8 |
      ((arr(offset + 7): Long) & 0xff)

  def longToBytes(long: Long, arr: Array[Byte] = new Array[Byte](8), offset: Int = 0): Array[Byte] = {
    arr(offset) = (long >> 56).asInstanceOf[Byte]
    arr(offset + 1) = (long >> 48).asInstanceOf[Byte]
    arr(offset + 2) = (long >> 40).asInstanceOf[Byte]
    arr(offset + 3) = (long >> 32).asInstanceOf[Byte]
    arr(offset + 4) = (long >> 24).asInstanceOf[Byte]
    arr(offset + 5) = (long >> 16).asInstanceOf[Byte]
    arr(offset + 6) = (long >> 8).asInstanceOf[Byte]
    arr(offset + 7) = (long).asInstanceOf[Byte]
    arr
  }

}