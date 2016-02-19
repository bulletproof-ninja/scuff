package scuff

object ThreadLocal {
  def apply[T](ctor: => T): java.lang.ThreadLocal[T] = new java.lang.ThreadLocal[T] {
    override final def initialValue: T = ctor
  }
}
