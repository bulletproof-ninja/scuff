package scuff.redis

import redis.clients.jedis._
import redis.clients.util.SafeEncoder._

class RedisStringKeyHashMap[V](name: String, connection: CONNECTION, serializer: scuff.Serializer[V] = new scuff.JavaSerializer[V])
  extends RedisHashMap[String, V](name, connection, RedisStringSerializer, serializer)

