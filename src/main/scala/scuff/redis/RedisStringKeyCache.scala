package scuff.redis

import redis.clients.jedis._
import redis.clients.util.SafeEncoder._

class RedisStringKeyCache[V](defaultTTL: Int, connection: CONNECTION, serializer: scuff.Serializer[V] = new scuff.JavaSerializer[V])
  extends RedisCache[String, V](defaultTTL, connection, RedisStringSerializer, serializer)