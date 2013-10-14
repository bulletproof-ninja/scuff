package scuff

import java.util.concurrent.TimeUnit

trait Clock {
  def precision: TimeUnit
  def now(implicit toPrecision: TimeUnit): Long
  @inline final def timestamp = new Timestamp(now(TimeUnit.MILLISECONDS))
  @inline final def durationSince(earlier: Long)(implicit toPrecision: TimeUnit): Long = now - earlier
  @inline final def durationUntil(future: Long)(implicit toPrecision: TimeUnit): Long = future - now
}

object Clock {
  val System = new Clock {
    @inline final def precision = TimeUnit.MILLISECONDS
    final def now(implicit toPrecision: TimeUnit): Long = toPrecision.convert(java.lang.System.currentTimeMillis(), precision)
  }
}

