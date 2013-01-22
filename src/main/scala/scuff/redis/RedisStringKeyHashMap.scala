package scuff.redis

import redis.clients.jedis._
import redis.clients.util.SafeEncoder._

class RedisStringKeyHashMap[V](name: String, connection: CONNECTION, serializer: scuff.Serializer[V] = new scuff.JavaSerializer[V])
  extends RedisHashMap[String, V](name, connection, StringKeySerializer, serializer)

private[redis] object StringKeySerializer extends scuff.Serializer[String] {
  def forth(str: String): Array[Byte] = encode(str)
  def back(array: Array[Byte]) = encode(array)
}
