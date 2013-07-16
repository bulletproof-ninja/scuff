package scuff.redis.util

import _root_.redis.clients.jedis._
import _root_.redis.clients.util.SafeEncoder
import scuff.redis._

class RedisStringKeyCache[V](defaultTTL: Int, connection: CONNECTION, serializer: scuff.Serializer[V] = new scuff.JavaSerializer[V])
  extends RedisCache[String, V](defaultTTL, connection, RedisStringSerializer, serializer)