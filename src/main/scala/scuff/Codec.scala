package scuff

import java.io._

/**
 * Coder/decoder interface.
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
}

trait Serializer[T] extends Codec[T, Array[Byte]]

trait StreamingSerializer[T] extends Serializer[T] {
  @inline def encode(t: T): Array[Byte] = {
    val out = new ByteArrayOutputStream
    encodeInto(out)(t)
    out.close()
    out.toByteArray()
  }
  @inline def decode(bytes: Array[Byte]) = decodeFrom(new ByteArrayInputStream(bytes))
  @inline protected final def asObjectInput(in: InputStream): ObjectInput = in match {
    case oi: ObjectInput ⇒ oi
    case _ ⇒ new ObjectInputStream(in)
  }
  @inline protected final def asObjectOutput(out: OutputStream): ObjectOutput = out match {
    case ou: ObjectOutput ⇒ ou
    case _ ⇒ new ObjectOutputStream(out)
  }
  def encodeInto(out: OutputStream)(t: T)
  def decodeFrom(in: InputStream): T
}

class JavaSerializer[T] extends StreamingSerializer[T] {
  @inline def encodeInto(out: OutputStream)(obj: T) = asObjectOutput(out).writeObject(obj)
  @inline def decodeFrom(in: InputStream): T = asObjectInput(in).readObject().asInstanceOf[T]
}

object JavaSerializer extends JavaSerializer[AnyRef]
