package scuff.eventual.redis

import concurrent.duration.{ Duration, DurationInt }

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream }

import redis.clients.jedis.Jedis
import redis.clients.util.SafeEncoder
import scuff.{ BitsBytes, ScuffByteArray, ScuffLong, StreamingSerializer, Timestamp }

/**
 * Keep track of handled [[scuff.eventual.EventSource#Transaction]]s, so process can resume
 * after shutdown.
 */
final class StringEventStreamTracker[ID](
    jedis: scuff.redis.CONNECTION,
    HashKey: String,
    clockSkew: Duration = 2.seconds)(implicit idCdc: scuff.Codec[ID, String]) {

  private final val SEP = ':'

  private[this] final val TimeKey = "T"

  implicit private def connection[T] = jedis.asInstanceOf[(Jedis ⇒ T) ⇒ T]

  @annotation.tailrec
  private def toLongWithRest(str: String, idx: Int = 0, acc: Long = 0): (Long, String) = {
    if (idx == str.length) {
      (acc, "")
    } else {
      val c = str.charAt(idx)
      if (c == SEP) {
        (acc, str.substring(idx + 1))
      } else {
        toLongWithRest(str, idx + 1, acc + (c - '0'))
      }
    }
  }
  @annotation.tailrec
  private def toLong(str: String, idx: Int = 0, acc: Long = 0): Long = {
    if (idx == str.length) {
      acc
    } else {
      val c = str.charAt(idx)
      if (c == SEP) {
        acc
      } else {
        toLong(str, idx + 1, acc + (c - '0'))
      }
    }
  }

  def resumeFrom: Option[scuff.Timestamp] = {
    connection { conn ⇒
      Option(conn.get(TimeKey)).map { str ⇒
        new Timestamp(toLong(str) - clockSkew.toMillis)
      }
    }
  }

  def nextExpectedRevision(streamId: ID): Long = {
    connection { conn ⇒
      conn.hget(HashKey, idCdc.encode(streamId)) match {
        case null ⇒ 0L
        case str ⇒ toLong(str) + 1L
      }
    }
  }

  def lookup[T](streamId: ID): Option[(Long, String)] = {
    connection { conn ⇒
      Option(conn.hget(HashKey, idCdc.encode(streamId))).map(toLongWithRest(_))
    }
  }

  /**
   * Mark stream/revision as processed.
   * @param streamId Transaction stream id
   * @param revision Transaction stream revision
   * @param time Transaction timestamp
   * @param state Optional update object.
   */
  def markAsProcessed[T](streamId: ID, revision: Long, time: Timestamp, state: String = null) {

    val valueStr = state match {
      case null ⇒ String valueOf revision
      case state ⇒
        val sb = new java.lang.StringBuilder
        sb append revision append SEP append state
        sb.toString
    }

    connection { conn ⇒
      val pl = conn.pipelined()
      pl.hset(HashKey, idCdc.encode(streamId), valueStr)
      pl.hset(HashKey, TimeKey, String valueOf time.asMillis)
      pl.sync()
    }
  }
}
