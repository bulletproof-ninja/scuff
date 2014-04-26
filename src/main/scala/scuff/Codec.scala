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

trait StreamingSerializer[T] extends Serializer[T] {
  @inline def encode(t: T): Array[Byte] = {
    val out = new ByteArrayOutputStream
    encodeInto(out)(t)
    out.toByteArray()
  }
  @inline def decode(bytes: Array[Byte]) = decodeFrom(new ByteArrayInputStream(bytes))
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
