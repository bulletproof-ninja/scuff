package scuff

import java.util.concurrent.TimeUnit

trait Clock {
  def precision: TimeUnit
  def now(implicit toPrecision: TimeUnit): Long
  @inline final def durationSince(earlier: Long)(implicit toPrecision: TimeUnit): Long = now - earlier
  @inline final def durationUntil(future: Long)(implicit toPrecision: TimeUnit): Long = future - now
}

object SystemClock extends Clock {
  @inline final def precision = TimeUnit.MILLISECONDS
  final def now(implicit toPrecision: TimeUnit): Long = toPrecision.convert(System.currentTimeMillis(), precision)
}