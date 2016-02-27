package scuff.redis

import _root_.redis.clients.jedis._
import _root_.redis.clients.util._

/**
 * Redis Hash implementation of [[scala.collection.concurrent.Map]].
 * NOTICE: This implementation does not support the CAS versions of `remove` and
 * `replace` due to inefficiencies such implementation would lead to.
 * @see [[scuff.redis.RedisMap]] if such functionality is needed.
 */
class RedisHashMap(name: String, conn: CONNECTION)
  extends BinaryRedisHashMap[String, String](name, conn, StringSerializer, StringSerializer) 

