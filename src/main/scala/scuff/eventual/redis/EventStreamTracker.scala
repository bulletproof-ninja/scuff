package scuff.eventual.redis

import concurrent.duration.{ Duration, DurationInt }
import java.io.{ ByteArrayInputStream, ByteArrayOutputStream }
import redis.clients.jedis.Jedis
import redis.clients.util.SafeEncoder
import scuff._

/**
 * Keep track of handled [[scuff.eventual.EventSource#Transaction]]s, so process can resume
 * after shutdown.
 */
final class EventStreamTracker[ID](
    jedis: redis.RedisConnectionPool,
    HashKey: String,
    clockSkew: Duration = 2.seconds)(implicit idCdc: Codec[ID, String]) {

  private final val SEP = ':'
  private final val SEP_str = String valueOf SEP

  private[this] final val TimeKey = "T"

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
  implicit private val StopOnSEP = new Numbers.Stopper {
    def apply(c: Char) = c == SEP
  }
  def resumeFrom: Option[Timestamp] = {
    jedis.connection { conn ⇒
      Option(conn.get(TimeKey)).map { str ⇒
        new Timestamp(str.parseLong() - clockSkew.toMillis)
      }
    }
  }

  def nextExpectedRevision(streamId: ID): Int = {
    jedis.connection { conn ⇒
      conn.hget(HashKey, idCdc.encode(streamId)) match {
        case null ⇒ 0
        case str ⇒ str.parseInt(StopOnSEP) + 1
      }
    }
  }

  def lookup[T](streamId: ID): Option[(Int, String)] = {
    jedis.connection { conn ⇒
      conn.hget(HashKey, idCdc.encode(streamId)) match {
        case null ⇒ None
        case str ⇒ Some(toIntWithMore(str))
      }
    }
  }

  /**
   * Mark stream/revision as processed.
   * @param streamId Transaction stream id
   * @param revision Transaction stream revision
   * @param time Transaction timestamp
   * @param state Optional update object.
   */
  def markAsProcessed[T](streamId: ID, revision: Int, time: Long, state: String = null) {

    val valueStr = state match {
      case null ⇒ String valueOf revision
      case state ⇒ String valueOf revision concat SEP_str concat state
    }

    jedis.pipeline { pl ⇒
      pl.hset(HashKey, idCdc.encode(streamId), valueStr)
      pl.hset(HashKey, TimeKey, String valueOf time)
    }
  }
}
