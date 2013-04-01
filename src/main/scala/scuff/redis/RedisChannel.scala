package scuff.redis

import redis.clients.jedis._
import redis.clients.util._

class RedisChannel[A](name: String, info: JedisShardInfo, serializer: scuff.Serializer[A] = new scuff.JavaSerializer[A]) extends scuff.Channel {
  type L = A ⇒ Unit
  private[this] val byteName = SafeEncoder.encode(name)
  def subscribe(subscriber: A ⇒ Unit) = {
    val jedisSubscriber = new BinaryJedisPubSub {
      def onMessage(channel: Array[Byte], msg: Array[Byte]) = try {
        subscriber(serializer.back(msg))
      } catch {
        case t: Throwable ⇒ t.printStackTrace(System.err)
      }
      def onPMessage(pattern: Array[Byte], channel: Array[Byte], msg: Array[Byte]) {}
      def onPSubscribe(channel: Array[Byte], noSubs: Int) {}
      def onPUnsubscribe(channel: Array[Byte], noSubs: Int) {}
      def onSubscribe(channel: Array[Byte], noSubs: Int) {}
      def onUnsubscribe(channel: Array[Byte], noSubs: Int) {}
    }
    val subscriptionThread = new Thread("Redis subscriber (channel: %s): %s".format(name, subscriber)) {
      val jedis = new BinaryJedis(info)
      override def run = try {
        jedis.connect()
        jedis.subscribe(jedisSubscriber, byteName)
      } catch {
        case i: InterruptedException ⇒ // Ok
        case t: Throwable ⇒ t.printStackTrace(System.err)
      } finally {
        jedis.disconnect()
      }
    }
    subscriptionThread.start()
    new scuff.Subscription {
      def cancel() = {
        jedisSubscriber.unsubscribe()
        subscriptionThread.interrupt()
      }
    }

  }
}