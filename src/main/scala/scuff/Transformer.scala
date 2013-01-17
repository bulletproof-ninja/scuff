package scuff

import java.io._

trait Transformer[A, B] {
  def forth(a: A): B
  def back(b: B): A
}

trait Serializer[T] extends Transformer[T, Array[Byte]] {
  def forth(t: T): Array[Byte] = {
    val out = new ByteArrayOutputStream
    streamForth(t, out)
    out.close()
    out.toByteArray()
  }
  def back(bytes: Array[Byte]) = streamBack(new ByteArrayInputStream(bytes))

  def streamForth(t: T, out: OutputStream)
  def streamBack(in: InputStream): T
}

class JavaSerializer[T] extends Serializer[T] {
  def streamForth(obj: T, out: OutputStream) = new ObjectOutputStream(out).writeObject(obj)
  def streamBack(in: InputStream): T = new ObjectInputStream(in).readObject().asInstanceOf[T]
}