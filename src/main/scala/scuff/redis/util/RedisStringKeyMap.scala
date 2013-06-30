package scuff.redis.util

import redis.clients.jedis._
import redis.clients.util.SafeEncoder._
import scuff.redis._

class RedisStringKeyMap[V](connection: CONNECTION, serializer: scuff.Serializer[V] = new scuff.JavaSerializer[V])
  extends RedisMap[String, V](connection, RedisStringSerializer, serializer)