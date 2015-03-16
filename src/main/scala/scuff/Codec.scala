package scuff

import java.io._
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

  val UTF8 = new Codec[String, Array[Byte]] {
    def encode(a: String) = a.utf8
    def decode(b: Array[Byte]) = b.utf8
  }

  def UTF8[A](codec: Codec[A, String]) = new Codec[A, Array[Byte]] {
    def encode(a: A) = codec.encode(a).utf8
    def decode(b: Array[Byte]) = codec.decode(b.utf8)
  }
  def UTF8[A](codec: Codec[A, Array[Byte]])(implicit dummy: ClassTag[A]) = new Codec[A, String] {
    def encode(a: A) = codec.encode(a).utf8
    def decode(b: String) = codec.decode(b.utf8)
  }
}

trait StreamingSerializer[T] extends Serializer[T] {

  def encodeInto(out: OutputStream)(t: T)
  def decodeFrom(in: InputStream): T

  def encode(t: T): Array[Byte] = {
    val out = new io.ByteOutputStream
    encodeInto(out)(t)
    out.toArray
  }

  def decode(bytes: Array[Byte]) = decodeFrom(new io.ByteInputStream(bytes))
  @inline protected final def asDataInput[T](in: InputStream)(handle: DataInput => T): T = in match {
    case oi: DataInput => handle(oi)
    case _ => handle(new DataInputStream(in))
  }
  @inline protected final def asDataOutput(out: OutputStream)(handle: DataOutput => Unit): Unit = {
    out match {
      case ou: DataOutput => handle(ou)
      case _ =>
        val ou = new DataOutputStream(out)
        handle(ou)
        ou.flush()
    }
  }
}

class JavaSerializer[T] extends StreamingSerializer[T] {
  def encodeInto(out: OutputStream)(obj: T) = out match {
    case oos: ObjectOutput =>
      oos.writeObject(obj)
    case _ =>
      val oos = new ObjectOutputStream(out)
      oos.writeObject(obj)
      oos.flush()
  }
  def decodeFrom(in: InputStream): T = {
    val obj = in match {
      case ois: ObjectInput => ois.readObject()
      case _ => new ObjectInputStream(in).readObject()
    }
    obj.asInstanceOf[T]
  }
}

object JavaSerializer extends JavaSerializer[AnyRef]
