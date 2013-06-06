package scuff.redis

import _root_.redis.clients.util.SafeEncoder

object RedisStringSerializer extends scuff.Serializer[String] {
  def forth(str: String): Array[Byte] = SafeEncoder.encode(str)
  def back(array: Array[Byte]) = SafeEncoder.encode(array)
}
