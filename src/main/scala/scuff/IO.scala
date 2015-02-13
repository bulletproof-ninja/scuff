package scuff

import java.io._

object IO {
  @annotation.tailrec
  private def transfer(in: InputStream, out: OutputStream, buffer: Array[Byte]) {
    in.read(buffer) match {
      case -1 ⇒ // Stop
      case 0 ⇒ transfer(in, out, buffer)
      case len ⇒
        out.write(buffer, 0, len)
        transfer(in, out, buffer)
    }
  }

  @annotation.tailrec
  private def transfer(in: Reader, out: Writer, buffer: Array[Char]) {
    in.read(buffer) match {
      case -1 ⇒ // Stop
      case 0 ⇒ transfer(in, out, buffer)
      case len ⇒
        out.write(buffer, 0, len)
        transfer(in, out, buffer)
    }
  }

  final val DefaultBufferSize = 16 * 1024

  /**
   * Copies from input to output stream,
   * until input stream is exhausted (returns -1 on read).
   * Output stream is neither flushed nor closed.
   */
  def copyStream(io: (InputStream, OutputStream), bufferSize: Int = DefaultBufferSize) {
    val (in, out) = io
    val buffer = new Array[Byte](bufferSize)
    try {
      transfer(in, out, buffer)
    } catch {
      case eof: EOFException ⇒ // Some faulty implementations throw EOF
    }
  }
  /**
   * Copies from reader to writer,
   * until reader is exhausted (returns -1 on read).
   * Writer is neither flushed nor closed.
   */
  def copyChars(io: (Reader, Writer), bufferSize: Int = DefaultBufferSize) {
    val (in, out) = io
    val buffer = new Array[Char](bufferSize)
    try {
      transfer(in, out, buffer)
    } catch {
      case eof: EOFException ⇒ // Some faulty implementations throw EOF
    }
  }
}
