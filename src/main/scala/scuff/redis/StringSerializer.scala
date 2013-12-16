package scuff.redis

import scuff.Serializer
import java.util.Arrays
import redis.clients.util.SafeEncoder

private[redis] object StringSerializer extends Serializer[String] {
  def encode(a: String) = SafeEncoder.encode(a)
  def decode(b: Array[Byte]) = SafeEncoder.encode(b)
}

class StringSerializer(prefix: String) extends Serializer[String] {
  private[this] val prefixBytes =
    if (prefix == null || prefix.length == 0) {
      Array.empty[Byte]
    } else if (prefix endsWith ":") {
      SafeEncoder.encode(prefix)
    } else {
      SafeEncoder.encode(prefix concat ":")
    }
  def encode(a: String) = {
    val encoded = RedisEncoder.encode(a)
    if (prefix.length == 0) {
      encoded
    } else {
      val withPrefix = new Array[Byte](prefixBytes.length + encoded.length)
      System.arraycopy(prefixBytes, 0, withPrefix, 0, withPrefix.length)
      System.arraycopy(encoded, 0, withPrefix, prefixBytes.length, encoded.length)
      withPrefix
    }
  }
  def decode(b: Array[Byte]) = RedisEncoder.encode(b, prefixBytes.length)
}
