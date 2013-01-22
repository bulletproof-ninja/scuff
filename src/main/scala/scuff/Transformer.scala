package scuff

import java.io._

trait Transformer[A, B] {
  def forth(a: A): B
  def back(b: B): A
}

trait Serializer[T] extends Transformer[T, Array[Byte]]

trait StreamingSerializer[T] extends Serializer[T] {
  @inline def forth(t: T): Array[Byte] = {
    val out = new ByteArrayOutputStream
    streamForth(out)(t)
    out.close()
    out.toByteArray()
  }
  @inline def back(bytes: Array[Byte]) = streamBack(new ByteArrayInputStream(bytes))
  @inline protected final def asObjectInput(in: InputStream): ObjectInput = in match {
    case oi: ObjectInput ⇒ oi
    case _ ⇒ new ObjectInputStream(in)
  }
  @inline protected final def asObjectOutput(out: OutputStream): ObjectOutput = out match {
    case ou: ObjectOutput ⇒ ou
    case _ ⇒ new ObjectOutputStream(out)
  }
  def streamForth(out: OutputStream)(t: T)
  def streamBack(in: InputStream): T
}

class JavaSerializer[T] extends StreamingSerializer[T] {
  @inline def streamForth(out: OutputStream)(obj: T) = asObjectOutput(out).writeObject(obj)
  @inline def streamBack(in: InputStream): T = asObjectInput(in).readObject().asInstanceOf[T]
}

object JavaSerializer extends JavaSerializer[Any]
