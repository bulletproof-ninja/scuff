package scuff.eventual.redis

import concurrent.duration.{ Duration, DurationInt }

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream }

import redis.clients.jedis.Jedis
import redis.clients.util.SafeEncoder
import scuff.{ BitsBytes, ScuffByteArray, ScuffInt, StreamingSerializer, Timestamp }

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
  private def toIntWithMore(str: String, idx: Int = 0, acc: Int = 0): (Int, String) = {
    if (idx == str.length) {
      (acc, "")
    } else {
      val c = str.charAt(idx)
      if (c == SEP) {
        (acc, str.substring(idx + 1))
      } else {
        toIntWithMore(str, idx + 1, acc * 10 + (c - '0'))
      }
    }
  }
  implicit private val StopOnSEP = new BitsBytes.Stopper {
    def apply(c: Char) = c == SEP
  }
  def resumeFrom: Option[scuff.Timestamp] = {
    connection { conn ⇒
      Option(conn.get(TimeKey)).map { str ⇒
        new Timestamp(BitsBytes.toLong(str) - clockSkew.toMillis)
      }
    }
  }

  def nextExpectedRevision(streamId: ID): Int = {
    connection { conn ⇒
      conn.hget(HashKey, idCdc.encode(streamId)) match {
        case null ⇒ 0
        case str ⇒ BitsBytes.toInt(str) + 1
      }
    }
  }

  def lookup[T](streamId: ID): Option[(Int, String)] = {
    connection { conn ⇒
      Option(conn.hget(HashKey, idCdc.encode(streamId))).map(toIntWithMore(_))
    }
  }

  /**
   * Mark stream/revision as processed.
   * @param streamId Transaction stream id
   * @param revision Transaction stream revision
   * @param time Transaction timestamp
   * @param state Optional update object.
   */
  def markAsProcessed[T](streamId: ID, revision: Int, time: Timestamp, state: String = null) {

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
