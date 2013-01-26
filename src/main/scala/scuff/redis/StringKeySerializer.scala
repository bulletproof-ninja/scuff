package scuff.redis

import _root_.redis.clients.util.SafeEncoder._

private[redis] object StringKeySerializer extends scuff.Serializer[String] {
  def forth(str: String): Array[Byte] = encode(str)
  def back(array: Array[Byte]) = encode(array)
}