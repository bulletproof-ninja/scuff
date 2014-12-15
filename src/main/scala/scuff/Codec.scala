package scuff

import java.io._
import java.util.Arrays
import scala.reflect.ClassTag

/**
 * Codec. Combined encoder/decoder interface.
 */
trait Codec[A, B] {
  def encode(a: A): B
  def decode(b: B): A
}

object Codec {
  private[this] val passthrough = new Codec[Any, Any] {
    def encode(a: Any) = a
    def decode(b: Any) = b
  }
  implicit def noop[T]: Codec[T, T] = passthrough.asInstanceOf[Codec[T, T]]

  def BinaryUTF8[A](codec: Codec[A, String]) = new Codec[A, Array[Byte]] {
    def encode(a: A) = codec.encode(a).utf8
    def decode(b: Array[Byte]) = codec.decode(b.utf8)
  }
  def BinaryUTF8[A](codec: Codec[A, Array[Byte]])(implicit dummy: ClassTag[A]) = new Codec[A, String] {
    def encode(a: A) = codec.encode(a).utf8
    def decode(b: String) = codec.decode(b.utf8)
  }
}

trait StreamingSerializer[T] extends Serializer[T] {

  private final class ByteInStream(bytes: Array[Byte], offset: Int = 0, len: Int = -1) extends InputStream {
    private[this] final val endIdx = if (len == -1) bytes.length - offset else len
    private[this] var idx = offset
    @inline
    override def available = endIdx - idx
    def read() = {
      if (available > 0) {
        idx += 1
        bytes(idx - 1)
      } else {
        -1
      }
    }
    override def read(into: Array[Byte]): Int = read(into, 0, into.length)
    override def read(into: Array[Byte], offset: Int, len: Int): Int = {
      if (available > 0) {
        val count = math.min(len, available)
        System.arraycopy(bytes, idx, into, 0, count)
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

  private final class ByteOutStream extends OutputStream {
    private[this] var bytes = new Array[Byte](256)
    private[this] var idx = 0
    def toArray = if (bytes.length == idx) bytes else Arrays.copyOf(bytes, idx)
    @inline
    private def remaining = bytes.length - idx
    private def resize(need: Int, proposed: Int = bytes.length * 2) {
      if (proposed - idx < need) {
        resize(need, proposed * 2)
      }
      bytes = Arrays.copyOf(bytes, proposed)
    }

    def write(b: Int) {
      if (idx == bytes.length) resize(1)
      bytes(idx) = b.asInstanceOf[Byte]
      idx += 1
    }
    override final def write(arr: Array[Byte]) {
      write(arr, 0, arr.length)
    }
    override final def write(arr: Array[Byte], offset: Int, len: Int) {
      if (remaining < len) {
        resize(len)
      }
      System.arraycopy(arr, offset, bytes, idx, len)
      idx += len
    }
  }

  def encode(t: T): Array[Byte] = {
    val out = new ByteOutStream
    encodeInto(out)(t)
    out.toArray
  }

  def decode(bytes: Array[Byte]) = decodeFrom(new ByteInStream(bytes))
  @inline protected final def asDataInput[T](in: InputStream)(handle: DataInputStream ⇒ T): T = in match {
    case oi: DataInputStream ⇒ handle(oi)
    case _ ⇒ handle(new DataInputStream(in))
  }
  @inline protected final def asDataOutput(out: OutputStream)(handle: DataOutputStream ⇒ Unit): Unit = {
    out match {
      case ou: DataOutputStream ⇒ handle(ou)
      case _ ⇒
        val ou = new DataOutputStream(out)
        handle(ou)
        ou.flush()
    }
  }
  def encodeInto(out: OutputStream)(t: T)
  def decodeFrom(in: InputStream): T
}

class JavaSerializer[T] extends StreamingSerializer[T] {
  @inline def encodeInto(out: OutputStream)(obj: T) = out match {
    case oos: ObjectOutput ⇒
      oos.writeObject(obj)
    case _ ⇒
      val oos = new ObjectOutputStream(out)
      oos.writeObject(obj)
      oos.flush()
  }
  @inline def decodeFrom(in: InputStream): T = {
    val obj = in match {
      case ois: ObjectInput ⇒ ois.readObject()
      case _ ⇒ new ObjectInputStream(in).readObject()
    }
    obj.asInstanceOf[T]
  }
}

object JavaSerializer extends JavaSerializer[AnyRef]
