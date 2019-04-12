package scuff.web

import scuff.MediaType
import scala.concurrent.duration.FiniteDuration
import java.net.URI
import java.net.URL
import java.time.format.DateTimeFormatter
import java.time.ZonedDateTime
import java.time.LocalDateTime
import java.time.Instant
import java.time.OffsetDateTime

object HttpHeaders {

  private def RFC_1123 = DateTimeFormatter.RFC_1123_DATE_TIME
  private def GMT = RFC_1123.getZone

  def RFC_1123(str: String): ZonedDateTime = ZonedDateTime.parse(str, RFC_1123)
  def RFC_1123(date: ZonedDateTime): String = {
    val gmt = if (date.getZone == GMT) date else date.withZoneSameInstant(GMT)
    RFC_1123.format(gmt)
  }
  def RFC_1123(epochMillis: Long): String = {
    val zdt = ZonedDateTime.ofInstant(Instant.ofEpochMilli(epochMillis), GMT)
    RFC_1123(zdt)
  }
  def RFC_1123(date: java.util.Date): String = {
    val zdt = ZonedDateTime.ofInstant(Instant.ofEpochMilli(date.getTime), GMT)
    RFC_1123(zdt)
  }
  def RFC_1123(date: LocalDateTime): String = RFC_1123(ZonedDateTime.of(date, GMT))
  def RFC_1123(date: OffsetDateTime): String = RFC_1123(date.atZoneSameInstant(GMT))

  final val LastModified = "Last-Modified"
  final val ContentLength = "Content-Length"
  final val ContentType = "Content-Type"
  final val ContentRange = "Content-Range"
  final val ETag = "ETag"
  final val Expect = "Expect"
  final val Age = "Age"
  final val IfNoneMatch = "If-None-Match"
  final val IfMatch = "If-Match"
  final val IfModifiedSince = "If-Modified-Since"
  final val CacheControl = "Cache-Control"
  final val Referer = "Referer"
  final val RetryAfter = "Retry-After"
  final val UserAgent = "User-Agent"
  final val Location = "Location"

  final def Location(location: URL): (String, String) = Location(location.toString)
  final def Location(location: URI): (String, String) = Location(location.toString)
  final def Location(location: String): (String, String) = Location -> location

  def LastModified(date: java.util.Date): (String, String) = LastModified -> RFC_1123(date)
  def LastModified(date: ZonedDateTime): (String, String) = LastModified -> RFC_1123(date)
  def LastModified(date: LocalDateTime): (String, String) = LastModified -> RFC_1123(date)
  def LastModified(date: OffsetDateTime): (String, String) = LastModified -> RFC_1123(date)

  def ContentLength(length: Long): (String, String) = ContentLength -> length.toString

  def ContentType(ct: MediaType): (String, String) = ContentType(ct.toString)
  def ContentType(ct: String): (String, String) = ContentType -> ct

  def ContentRange(unit: String, range: Range, size: Long): (String, String) =
    ContentRange -> s"""$unit ${range.head}-${range.last}/$size"""
  def ContentRange(unit: String, size: Long): (String, String) =
    ContentRange -> s"""$unit */$size"""
  def ContentRange(unit: String, range: Range): (String, String) =
    ContentRange -> s"""$unit ${range.head}-${range.last}/*"""

  def ETag(etag: scuff.web.ETag): (String, String) = ETag -> etag.headerString

  def Age(age: Int): (String, String) = Age -> age.toString

  def RetryAfter(dur: FiniteDuration): (String, String) = RetryAfter(dur.toSeconds)
  def RetryAfter(dur: java.time.Duration): (String, String) = RetryAfter(dur.toMillis / 1000)
  def RetryAfter(seconds: Long): (String, String) = RetryAfter -> seconds.toString
  def RetryAfter(date: ZonedDateTime): (String, String) = RetryAfter -> RFC_1123(date)
  def RetryAfter(date: java.util.Date): (String, String) = RetryAfter -> RFC_1123(date)
  def RetryAfter(date: LocalDateTime): (String, String) = RetryAfter -> RFC_1123(date)
  def RetryAfter(date: OffsetDateTime): (String, String) = RetryAfter -> RFC_1123(date)

}
