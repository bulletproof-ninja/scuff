package scuff.redis

import redis.clients.jedis._
import redis.clients.util._
import java.net._
import redis.clients.util.SafeEncoder

final class RedisPublisher(channelName: String, connection: CONNECTION)
  extends BinaryRedisPublisher(channelName, connection, StringSerializer)

