package scuff.redis.util

import redis.clients.jedis._
import redis.clients.util.SafeEncoder._
import scuff.redis.RedisHashMap
import scuff.redis._

class RedisStringKeyHashMap[V](name: String, connection: CONNECTION, serializer: scuff.Serializer[V] = new scuff.JavaSerializer[V])
  extends RedisHashMap[String, V](name, connection, RedisStringSerializer, serializer)

