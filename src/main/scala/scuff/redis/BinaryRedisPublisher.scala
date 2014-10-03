package scuff.redis

import redis.clients.jedis.{ MultiKeyBinaryCommands, MultiKeyBinaryRedisPipeline }
import redis.clients.util.SafeEncoder
import scuff.Codec

class BinaryRedisPublisher[T](channelName: String, serializer: Codec[T, Array[Byte]]) {
  require(channelName != null && channelName.length > 0, "Must have channel name")
  require(serializer != null, "Must have Codec")

  private[this] val byteName = SafeEncoder.encode(channelName)

  /**
   *  Publish using connection.
   *  @return Number of receivers.
   */
  @annotation.implicitNotFound("Cannot find implicit Jedis or Pipeline")
  def publish(msg: T)(implicit conn: PublishMagnet): Unit = conn match {
    case Right(pl) => pl.publish(byteName, serializer.encode(msg))
    case Left(jedis) => jedis.publish(byteName, serializer.encode(msg))
  }

}
