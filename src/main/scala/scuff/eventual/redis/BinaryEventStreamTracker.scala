package scuff.eventual.redis

import concurrent.duration.{ Duration, DurationInt }

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream }

import redis.clients.jedis.Jedis
import redis.clients.util.SafeEncoder
import scuff.{ BitsBytes, ScuffByteArray, ScuffLong, ScuffInt, StreamingSerializer, Timestamp }

/**
 * Keep track of handled [[scuff.eventual.EventSource#Transaction]]s, so process can resume
 * after shutdown.
 */
final class BinaryEventStreamTracker[ID](
    jedis: scuff.redis.CONNECTION,
    hashName: String,
    clockSkew: Duration = 2.seconds)(implicit idCdc: scuff.Serializer[ID]) {

  private[this] final val TimeKey = Array[Byte]('t')
  private[this] final val HashKey = SafeEncoder.encode(hashName)

  implicit private def connection[T] = jedis.asInstanceOf[(Jedis ⇒ T) ⇒ T]

  def resumeFrom: Option[scuff.Timestamp] = {
    connection { conn ⇒
      Option(conn.get(TimeKey)).map(arr ⇒ new Timestamp(BitsBytes.bytesToInt(arr) - clockSkew.toMillis))
    }
  }

  def nextExpectedRevision(streamId: ID): Int = {
    connection { conn ⇒
      conn.hget(HashKey, idCdc.encode(streamId)) match {
        case null ⇒ 0
        case arr ⇒ arr.toInt + 1
      }
    }
  }

  def lookup[T](streamId: ID)(implicit tCdc: StreamingSerializer[T]): Option[(Int, Option[T])] = {
    connection { conn ⇒
      conn.hget(HashKey, idCdc.encode(streamId)) match {
        case null ⇒ None
        case arr ⇒
          val rev = arr.toInt
          if (arr.length == 8) {
            Some(rev -> None)
          } else {
            val in = new ByteArrayInputStream(arr, 8, arr.length - 8)
            val t = tCdc.decodeFrom(in)
            Some(rev -> Some(t))
          }
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
  def markAsProcessed[T](streamId: ID, revision: Int, time: Timestamp, state: T = null)(implicit tCdc: StreamingSerializer[T] = null) {
    val revBytes = revision.toByteArray
    val timeBytes = time.asMillis.toByteArray

    val updateBytes = state match {
      case null ⇒ revBytes
      case update ⇒
        if (tCdc == null) throw new IllegalArgumentException(s"Serializer for $update not provided.")
        val bao = new ByteArrayOutputStream(1024)
        bao.write(revBytes)
        tCdc.encodeInto(bao)(update)
        bao.toByteArray()
    }

    connection { conn ⇒
      val pl = conn.pipelined()
      pl.hset(HashKey, idCdc.encode(streamId), updateBytes)
      pl.hset(HashKey, TimeKey, timeBytes)
      pl.sync()
    }
  }
}
