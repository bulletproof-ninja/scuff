package scuff.redis

import redis.clients.jedis._
import redis.clients.util.SafeEncoder._

class RedisStringKeyMap[V](connection: CONNECTION, serializer: scuff.Serializer[V] = new scuff.JavaSerializer[V])
  extends RedisMap[String, V](connection, RedisStringSerializer, serializer)