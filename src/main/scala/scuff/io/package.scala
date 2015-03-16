package scuff

import java.io._

package object io {

  final val DefaultBufferSize = 8 * 1024

  implicit final class ScuffInputStream(val in: InputStream) extends AnyVal {
    @annotation.tailrec
    private def transfer(in: InputStream, out: OutputStream, buffer: Array[Byte]) {
      in.read(buffer) match {
        case -1 => // Stop
        case 0 => transfer(in, out, buffer)
        case len =>
          out.write(buffer, 0, len)
          transfer(in, out, buffer)
      }
    }
    /**
     * Copies to output stream, until input stream is exhausted (returns -1 on read).
     * Output stream is neither flushed nor closed.
     */
    def copyTo(out: OutputStream, bufferSize: Int = DefaultBufferSize) {
      val buffer = new Array[Byte](bufferSize)
      try {
        transfer(in, out, buffer)
      } catch {
        case eof: EOFException => // Some faulty implementations throw EOF
      }
    }
  }

  implicit final class ScuffReader(val in: Reader) extends AnyVal {
    @annotation.tailrec
    private def transfer(in: Reader, out: Writer, buffer: Array[Char]) {
      in.read(buffer) match {
        case -1 => // Stop
        case 0 => transfer(in, out, buffer)
        case len =>
          out.write(buffer, 0, len)
          transfer(in, out, buffer)
      }
    }
    /**
     * Copies from reader to writer,
     * until reader is exhausted (returns -1 on read).
     * Writer is neither flushed nor closed.
     */
    def copyTo(out: Writer, bufferSize: Int = DefaultBufferSize) {
      val buffer = new Array[Char](bufferSize)
      try {
        transfer(in, out, buffer)
      } catch {
        case eof: EOFException => // Some faulty implementations throw EOF
      }
    }
  }

}
