package scuff

import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeUnit._

trait Clock {
  def precision: TimeUnit
  def now(): Long
  @inline final def now(toPrecision: TimeUnit): Long = {
    if (toPrecision eq this.precision) now()
    else toPrecision.convert(now(), precision)
  }
  @inline final def durationSince(earlier: Long, withPrecision: TimeUnit = precision): Long = now(withPrecision) - earlier
  @inline final def durationUntil(future: Long, withPrecision: TimeUnit = precision): Long = future - now(withPrecision)
}

object Clock {
  val System = new Clock {
    @inline def precision = MILLISECONDS
    @inline def now() = java.lang.System.currentTimeMillis
  }
  val NanoTimer = new Clock {
    @inline def precision = NANOSECONDS
    @inline def now(): Long = java.lang.System.nanoTime
  }
}

