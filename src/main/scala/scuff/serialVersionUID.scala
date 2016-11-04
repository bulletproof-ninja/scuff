package scuff

import scala.reflect.{ ClassTag, classTag }

object serialVersionUID {
  private[this] val serialUIDCache = new ClassValue[Long] {
    def computeValue(cls: Class[_]): Long = {
      scala.util.Try(cls.getDeclaredField("serialVersionUID")).map { serialField =>
        serialField.setAccessible(true)
        serialField.get(cls).asInstanceOf[Long]
      } getOrElse {
        sys.error(s"No `serialVersionUID` defined on $cls")
      }
    }
  }
  def apply(cls: Class[_]): Long = serialUIDCache.get(cls)
  def apply[T <: AnyRef: ClassTag]: Long = serialUIDCache.get(classTag[T].runtimeClass)
}
