package scuff

import java.lang.reflect.Method

/**
  * Generic collector of metrics.
  */
trait MetricsCollector[@specialized(Long, Int, AnyRef) V] {
  /** Generate timestamp. */
  def timestamp(): Long
  /**
    *  Report value and optional timestamp.
    *  @param name Some name, either unique or qualifier for method
    *  @param method Optional method, can be used to construct fully qualified name, defaults to `null`.
    *  @param value The value being reported
    *  @param time The timestamp, defaults to calling [[MetricsCollector.timestamp]]
    */
  def report(
      name: String, method: Method = null)(
      value: V, time: Long = timestamp): Unit
}

private object TimeCollector {
  private[this] val NamesAndMethods = new ClassValue[(String, Method)] {
    def computeValue(cls: Class[_]) = cls.getSimpleName -> cls.getEnclosingMethod
  }
  @inline final def NameAndMethod(cls: Class[_]) = NamesAndMethods.get(cls)
}

/**
  * Convenience trait for collection of timing metrics.
  */
trait TimeCollector extends MetricsCollector[Long] {
  import TimeCollector._
  import scala.util.control.NonFatal

  /**
    * Times and reports the thunk, using identifier
    * as reporting name, with the method defining the thunk.
    */
  @inline
  final def reportTiming[R](identifier: String)(thunk: => R): R = {
    val thunkClass = (thunk _).getClass
    val (name, method) = NameAndMethod(thunkClass)
    val id = identifier match {
      case null | "" => name
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
  /**
    * Convenience method, that times and reports the thunk,
    * and uses the containing class name as reporting name,
    * with the method defining the thunk.
    */
  @inline
  final def reportTiming[R](thunk: => R): R = reportTiming(null)(thunk)
}
