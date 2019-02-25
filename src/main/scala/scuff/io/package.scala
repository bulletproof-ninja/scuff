package scuff

import java.io._

package object io {

  final val DefaultBufferSize = 8 * 1024

  implicit class ScuffInputStream(private val in: InputStream) extends AnyVal {
    @annotation.tailrec
    private def transfer(in: InputStream, out: OutputStream, buffer: Array[Byte]): Unit = {
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
    def copyTo(out: OutputStream, bufferSize: Int = DefaultBufferSize): Unit = copyTo(out, new Array[Byte](bufferSize))
    /**
      * Copies to output stream, until input stream is exhausted (returns -1 on read).
      * Output stream is neither flushed nor closed.
      */
    def copyTo(out: OutputStream, buffer: Array[Byte]): Unit = {
      try {
        transfer(in, out, buffer)
      } catch {
        case _: EOFException => // Some faulty implementations throw EOF
      }
    }
  }

  implicit class ScuffReader(private val in: Reader) extends AnyVal {
    @annotation.tailrec
    private def transfer(in: Reader, out: Writer, buffer: Array[Char]): Unit = {
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
    def copyTo(out: Writer, bufferSize: Int = DefaultBufferSize): Unit = copyTo(out, new Array[Char](bufferSize))
    /**
      * Copies from reader to writer,
      * until reader is exhausted (returns -1 on read).
      * Writer is neither flushed nor closed.
      */
    def copyTo(out: Writer, buffer: Array[Char]): Unit = {
      try {
        transfer(in, out, buffer)
      } catch {
        case _: EOFException => // Some faulty implementations throw EOF
      }
    }
    def copyToCharSeq(bufferSize: Int = DefaultBufferSize): CharSequence = copyToCharSeq(new Array[Char](bufferSize))
    def copyToCharSeq(buffer: Array[Char]): CharSequence = {
      @annotation.tailrec
      def copyToStringBuilder(sb: java.lang.StringBuilder): java.lang.StringBuilder = {
        in.read(buffer) match {
          case -1 => sb
          case bytesRead =>
            if (bytesRead > 0) sb.append(buffer, 0, bytesRead)
            copyToStringBuilder(sb)
        }
      }
      copyToStringBuilder(new java.lang.StringBuilder(256))
    }
  }

}
