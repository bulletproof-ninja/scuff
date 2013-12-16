package scuff.redis

import redis.clients.jedis.Protocol

object RedisEncoder {
  @inline
  def encode(str: String) = str.getBytes(Protocol.CHARSET)
  @inline
  def encode(bytes: Array[Byte], offset: Int = 0, length: Int = -1) = {
    val len = if (length == -1) bytes.length - offset else length
    String.valueOf(bytes, offset, len, Protocol.CHARSET)
  }
}