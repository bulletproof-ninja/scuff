package scuff.io

import java.io.OutputStream
import java.util.Arrays

/**
 * Unsynchronized `java.io.OutputStream`.
 */
final class ByteOutputStream(initSize: Int = 512) extends OutputStream {
  private[this] var bytes = new Array[Byte](initSize)
  private[this] var idx = 0
  def toArray = if (bytes.length == idx) bytes else Arrays.copyOf(bytes, idx)
  @inline
  private def remaining = bytes.length - idx
  private def resize(need: Int, proposed: Int = bytes.length * 2): Unit = {
    if (proposed - idx < need) {
      resize(need, proposed * 2)
    }
    bytes = Arrays.copyOf(bytes, proposed)
  }
  def write(b: Int): Unit = {
    if (idx == bytes.length) resize(1)
    bytes(idx) = b.asInstanceOf[Byte]
    idx += 1
  }
  override def write(arr: Array[Byte]): Unit = {
    write(arr, 0, arr.length)
  }
  override def write(arr: Array[Byte], offset: Int, len: Int): Unit = {
    if (remaining < len) {
      resize(len)
    }
    System.arraycopy(arr, offset, bytes, idx, len)
    idx += len
  }
}
