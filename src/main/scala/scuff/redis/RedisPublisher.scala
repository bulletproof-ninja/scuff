package scuff.redis

import redis.clients.jedis._
import redis.clients.util._
import java.net._
import redis.clients.util.SafeEncoder

final class RedisPublisher(connection: CONNECTION) {
  def publish[T](channelName: String, serializer: scuff.Serializer[T])(msg: T) {
    connection(_.publish(SafeEncoder.encode(channelName), serializer.encode(msg)))
  }
}
