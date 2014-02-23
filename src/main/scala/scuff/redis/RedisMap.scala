package scuff.redis

import redis.clients.jedis._
import redis.clients.util.SafeEncoder._

/**
 * Redis general implementation of [[scala.collection.mutable.ConcurrentMap]].
 * @see [[scuff.redis.RedisHashMap]] for Hash implementation.
 */
class RedisMap(conn: CONNECTION, keyPrefix: String = "")
  extends BinaryRedisMap(conn, new StringSerializer(keyPrefix), StringSerializer)
