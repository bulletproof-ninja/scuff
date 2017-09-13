package scuff

import java.io._

/**
  * Codec. Combined encoder/decoder interface.
  */
trait Codec[A, B] extends Serializable {
  def encode(a: A): B
  def decode(b: B): A

  val reverse: Codec[B, A] = new Codec[B, A] {
    def encode(b: B): A = Codec.this.decode(b)
    def decode(a: A): B = Codec.this.encode(a)
  }

}

object Codec {
  import scala.reflect.ClassTag

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
  def UTF8[A: ClassTag](codec: Codec[A, Array[Byte]]) = new Codec[A, String] {
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
  @inline protected final def asDataInput[O](in: InputStream)(handle: DataInput => O): O = in match {
    case in: DataInput => handle(in)
    case _ => handle(new DataInputStream(in))
  }
  @inline protected final def asDataOutput(out: OutputStream)(handle: DataOutput => Unit): Unit = {
    out match {
      case out: DataOutput => handle(out)
      case _ =>
        val dout = new DataOutputStream(out)
        handle(dout)
        dout.flush()
    }
  }
  @inline protected final def asObjectInput[O](in: InputStream)(handle: ObjectInput => O): O = in match {
    case in: ObjectInput => handle(in)
    case _ => handle(new ObjectInputStream(in))
  }
  @inline protected final def asObjectOutput(out: OutputStream)(handle: ObjectOutput => Unit): Unit = {
    out match {
      case out: ObjectOutput => handle(out)
      case _ =>
        val dout = new ObjectOutputStream(out)
        handle(dout)
        dout.flush()
    }
  }
}

class JavaSerializer[T] extends StreamingSerializer[T] {
  def encodeInto(out: OutputStream)(obj: T) = asObjectOutput(out)(_.writeObject(obj))
  def decodeFrom(in: InputStream): T = asObjectInput(in)(_.readObject().asInstanceOf[T])
}

object JavaSerializer extends JavaSerializer[AnyRef]
