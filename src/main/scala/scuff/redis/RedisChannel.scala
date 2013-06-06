package scuff.redis

import redis.clients.jedis._
import redis.clients.util._
import java.util.concurrent._

class RedisChannel[A](channelName: String, info: JedisShardInfo, subscriberThread: Executor, serializer: scuff.Serializer[A]) extends scuff.Channel {

  def this(channelName: String, info: JedisShardInfo, subscriberThreadFactory: ThreadFactory, serializer: scuff.Serializer[A]) =
    this(channelName, info, Executors.newSingleThreadExecutor(subscriberThreadFactory), serializer)

  def this(channelName: String, info: JedisShardInfo, serializer: scuff.Serializer[A] = new scuff.JavaSerializer[A]) =
    this(channelName, info, scuff.ThreadFactory(classOf[RedisChannel[_]].getName), serializer)

  type F = Nothing
  type L = A ⇒ Unit
  private[this] val byteName = SafeEncoder.encode(channelName)
  def subscribe(subscriber: L, filter: Nothing ⇒ Boolean) = {
    val jedisSubscriber = new BinaryJedisPubSub {
      def onMessage(channel: Array[Byte], msg: Array[Byte]) = subscriber(serializer.back(msg))
      def onPMessage(pattern: Array[Byte], channel: Array[Byte], msg: Array[Byte]) {}
      def onPSubscribe(channel: Array[Byte], noSubs: Int) {}
      def onPUnsubscribe(channel: Array[Byte], noSubs: Int) {}
      def onSubscribe(channel: Array[Byte], noSubs: Int) {}
      def onUnsubscribe(channel: Array[Byte], noSubs: Int) {}
    }
    subscriberThread execute new Runnable {
      val jedis = new BinaryJedis(info)
      override def run = try {
        jedis.connect()
        jedis.subscribe(jedisSubscriber, byteName)
      } finally {
        jedis.disconnect()
      }
    }
    new scuff.Subscription {
      def cancel() = jedisSubscriber.unsubscribe()
      }

    }
}
