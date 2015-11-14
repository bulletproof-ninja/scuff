package scuff.io

import java.io.InputStream

/**
  * Unsynchronized [[InputStream]].
  */
final class ByteInputStream(bytes: Array[Byte], offset: Int = 0, len: Int = -1) extends InputStream {
  private[this] final val endIdx = if (len == -1) bytes.length - offset else len
  private[this] var idx = offset
  @inline override def available = endIdx - idx
  def read(): Int = {
    if (available > 0) {
      idx += 1
      bytes(idx - 1) & 0xff
    } else {
      -1
    }
  }
  override def read(into: Array[Byte]): Int = read(into, 0, into.length)
  override def read(into: Array[Byte], offset: Int, len: Int): Int = {
    if (available > 0) {
      val count = math.min(len, available)
      System.arraycopy(bytes, idx, into, offset, count)
      idx += count
      count
    } else {
      -1
    }
  }
  override def skip(count: Long): Long = {
    val skip = math.min(available, count)
    idx += skip.asInstanceOf[Int]
    skip
  }
}
