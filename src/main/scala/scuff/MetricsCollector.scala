package scuff

import java.lang.reflect.Method
import scala.util.control.NonFatal

/**
  * Generic collector of metrics.
  */
trait MetricsCollector[@specialized(Long, Int) N] {
  /** Report value. */
  def report(name: String, value: N, method: Method = null)(implicit num: Numeric[N]): Unit
}

private object TimeCollector {
  private[this] val NamesAndMethods = new ClassValue[(String, Method)] {
    def computeValue(cls: Class[_]) = cls.getSimpleName -> cls.getEnclosingMethod
  }
  @inline final def NameAndMethod(cls: Class[_]) = NamesAndMethods.get(cls)
}

/**
  * Collector of timing metrics.
  */
trait TimeCollector extends MetricsCollector[Long] {

  protected def timestamp(): Long

  @inline
  final def reportTiming[R](identifier: String)(thunk: => R): R = {
    val thunkClass = (thunk _).getClass
    val (name, method) = TimeCollector.NameAndMethod(thunkClass)
    val id = identifier match {
      case null => name
      case identifier => identifier
    }
    val start = timestamp()
    val result = thunk
    val duration = timestamp() - start
    try report(id, duration, method) catch {
      case NonFatal(_) => // Ignore
    }
    result
  }
  @inline
  final def reportTiming[R](thunk: => R): R = reportTiming(null)(thunk)
}
