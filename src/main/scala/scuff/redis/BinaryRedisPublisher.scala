package scuff.redis

import redis.clients.jedis._
import redis.clients.util._
import java.net._
import redis.clients.util.SafeEncoder
import scuff.Serializer

class BinaryRedisPublisher[T](channelName: String, connection: CONNECTION, serializer: Serializer[T]) {
  private[this] val byteName = SafeEncoder.encode(channelName)
  def publish(msg: T)(implicit conn: Jedis = null) {
    if (conn == null) {
      connection(publishWith(_, msg))
    } else {
      publishWith(conn, msg)
    }
  }
  private def publishWith(conn: Jedis, msg: T) {
    conn.publish(byteName, serializer.encode(msg))
  }
}
