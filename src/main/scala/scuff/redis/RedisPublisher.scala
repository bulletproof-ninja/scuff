package scuff.redis

import redis.clients.jedis._
import java.net._
import redis.clients.util.SafeEncoder

class RedisPublisher(info: JedisShardInfo) {
  private[this] val jedis = new BinaryJedis(info)
  def publish[T](channelName: String, serializer: scuff.Serializer[T])(msg: T): Unit = jedis.publish(SafeEncoder.encode(channelName), serializer.forth(msg))
}
