package scuff.redis.util

import _root_.redis.clients.util.SafeEncoder

object RedisStringSerializer extends scuff.Serializer[String] {
  def encode(str: String): Array[Byte] = SafeEncoder.encode(str)
  def decode(array: Array[Byte]) = SafeEncoder.encode(array)
}
