package scuff.redis

import redis.clients.jedis._
import redis.clients.util._
import java.net._
import redis.clients.util.SafeEncoder

final class RedisPublisher(channelName: String)
  extends BinaryRedisPublisher(channelName, StringSerializer)

