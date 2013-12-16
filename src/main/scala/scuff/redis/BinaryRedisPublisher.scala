package scuff.redis

import redis.clients.jedis._
import redis.clients.util._
import java.net._
import redis.clients.util.SafeEncoder
import scuff.Serializer

class BinaryRedisPublisher[T](channelName: String, connection: CONNECTION, serializer: Serializer[T]) {
  private[this] val byteName = SafeEncoder.encode(channelName)
  def publish(msg: T) {
    connection(_.publish(byteName, serializer.encode(msg)))
  }
}
