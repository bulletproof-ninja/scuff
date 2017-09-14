package scuff

import java.lang.reflect.Method

/**
  * Generic collector of metrics.
  */
trait MetricsCollector[@specialized(Long, Int) V] {
  /** Generate timestamp. */
  def timestamp(): Long
  /** Report value. */
  def report(name: String, method: Method = null)(value: V, time: Long = timestamp): Unit
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
  import scala.util.control.NonFatal

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
    val now = timestamp()
    val duration = now - start
    try report(id, method)(duration, now) catch {
      case NonFatal(_) => // Ignore
    }
    result
  }
  @inline
  final def reportTiming[R](thunk: => R): R = reportTiming(null)(thunk)
}
