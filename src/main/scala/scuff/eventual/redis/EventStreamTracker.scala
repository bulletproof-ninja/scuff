package scuff.eventual.redis

import concurrent.Future
import concurrent.duration.{ Duration, DurationInt }
import scuff.{ Codec, Numbers, ScuffString, Timestamp, redis }
import _root_.redis.clients.jedis.Jedis

/**
 * Keep track of handled [[scuff.eventual.EventSource#Transaction]]s,
 * which is helpful for durable subscribers. For simplicity, a
 * data string is attached to each update, for keeping state.
 */
final class EventStreamTracker[ID](
    HashKey: String,
    clockSkew: Duration = 2.seconds)(implicit idCdc: Codec[ID, String]) {

  private final val SEP = ':'
  private final val SEP_str = String valueOf SEP

  private[this] final val TimeKey = "T"

  implicit private[this] final val TenSeconds = 10.seconds

  @annotation.tailrec
  private def unsafeIntAndData(str: String, idx: Int = 0, acc: Int = 0): (Int, String) = {
    if (idx == str.length) {
      (acc, "")
    } else {
      val c = str.charAt(idx)
      if (c == SEP) {
        (acc, str.substring(idx + 1))
      } else {
        unsafeIntAndData(str, idx + 1, acc * 10 + (c - '0'))
      }
    }
  }

  def resumeFrom(implicit conn: Jedis): Option[Timestamp] =
    Option(conn.get(TimeKey)).map { str ⇒
      new Timestamp(str.toLong - clockSkew.toMillis)
    }

  def lookupRevision[T](streamId: ID)(implicit conn: Jedis): Option[(Int, String)] =
    conn.hget(HashKey, idCdc.encode(streamId)) match {
      case null ⇒ None
      case str ⇒ new Some(unsafeIntAndData(str))
    }

  private def toMap(streamId: ID, revision: Int, time: Long, state: String): java.util.Map[String, String] = {
    val streamKey = idCdc encode streamId
    val valueStr = state.length match {
      case 0 ⇒ String valueOf revision
      case _ ⇒ String valueOf revision concat SEP_str concat state
    }
    val map = new java.util.HashMap[String, String](4)
    map.put(streamKey, valueStr)
    map.put(TimeKey, String valueOf time)
    map
  }

  def process(streamId: ID, revision: Int, time: Long)(updater: String ⇒ String)(implicit conn: Jedis): String = {
    val value = conn.hget(HashKey, idCdc.encode(streamId)) match {
      case null ⇒ ""
      case str ⇒ unsafeIntAndData(str)._2
    }
    val updated = updater(value).trim
    conn.hmset(HashKey, toMap(streamId, revision, time, updated))
    updated
  }

  def markAsProcessed(streamId: ID, revision: Int, time: Long)(implicit conn: Jedis) {
    conn.hmset(HashKey, toMap(streamId, revision, time, ""))
  }
}
