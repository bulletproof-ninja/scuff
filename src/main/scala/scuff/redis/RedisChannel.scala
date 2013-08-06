package scuff.redis

import scuff._
import _root_.redis.clients.jedis._
import _root_.redis.clients.util._
import java.util.concurrent._
import collection.JavaConverters._
import java.util.concurrent.locks._

/**
 * Redis pub/sub channel.
 */
class RedisChannel[A] private (
  channelName: Array[Byte],
  info: JedisShardInfo,
  subscriberThread: Executor,
  serializer: Serializer[A],
  publishCtx: concurrent.ExecutionContext)
    extends Channel {

  type F = A
  type L = A ⇒ Unit
  private[this] val byteName = SafeEncoder.encode(channelName)

  private val (shared, exclusive, newSubscriber) = {
    val rwLock = new ReentrantReadWriteLock
    val exclusive = rwLock.writeLock
    (rwLock.readLock, exclusive, exclusive.newCondition)
  }
  private class FilteredSubscriber(sub: L, doTell: F ⇒ Boolean) {
    def tell(a: A) = if (doTell(a)) {
      publishCtx execute new Runnable { def run = sub(a) }
    }
  }
  private[this] val subscribers = collection.mutable.Buffer[FilteredSubscriber]()

  private[this] val jedisSubscriber = new BinaryJedisPubSub {
    def onMessage(channel: Array[Byte], byteMsg: Array[Byte]) {
      val msg = serializer.decode(byteMsg)
      shared.whenLocked {
        subscribers.foreach(_.tell(msg))
      }
    }
    def onPMessage(pattern: Array[Byte], channel: Array[Byte], msg: Array[Byte]) {}
    def onPSubscribe(channel: Array[Byte], noSubs: Int) {}
    def onPUnsubscribe(channel: Array[Byte], noSubs: Int) {}
    def onSubscribe(channel: Array[Byte], noSubs: Int) {}
    def onUnsubscribe(channel: Array[Byte], noSubs: Int) {}
  }

  private[this] val jedis = new BinaryJedis(info)
  def subscribe(subscriber: L, filter: A ⇒ Boolean) = {
    val filteredSub = new FilteredSubscriber(subscriber, filter)
    exclusive.whenLocked {
      if (subscribers.isEmpty) {
        newSubscriber.signal()
      }
      subscribers += filteredSub
    }
    new Subscription {
      def cancel() = exclusive.whenLocked {
        subscribers -= filteredSub
        if (subscribers.isEmpty) {
          jedisSubscriber.unsubscribe()
        }
      }
    }
  }

  private def start() {
    subscriberThread execute new Runnable {
      def run = while (!Thread.currentThread.isInterrupted) try {
        awaitSubscribers()
        consumeMessages()
      } catch {
        case _: InterruptedException ⇒ Thread.currentThread().interrupt()
        case e: Exception ⇒ publishCtx.reportFailure(e)
      }

      def awaitSubscribers() = exclusive.whenLocked {
        while (subscribers.isEmpty) {
          newSubscriber.await()
        }
      }

      def consumeMessages() = try {
        jedis.connect()
        jedis.subscribe(jedisSubscriber, channelName)
      } finally {
        jedis.disconnect()
      }
    }
  }

}

object RedisChannel {

  /**
   * @param server Redis server information
   * @param jedisSubscriberThread Subscription thread.
   * This thread will be monopolized by Jedis, therefore,
   * the `Executor` should not be a general purpose thread-pool.
   * @param serializer The byte array decoder
   * @param publishCtx The execution context used to publish messages
   */
  def apply[A](
    channelName: String, server: JedisShardInfo, jedisSubscriberThread: Executor,
    serializer: Serializer[A], publishCtx: concurrent.ExecutionContext): RedisChannel[A] = {
    val rc = new RedisChannel(SafeEncoder.encode(channelName), server, jedisSubscriberThread, serializer, publishCtx)
    rc.start()
    rc
  }

  def apply[A](channelName: String, server: JedisShardInfo, subscriberThreadFactory: java.util.concurrent.ThreadFactory,
    serializer: Serializer[A], publishCtx: concurrent.ExecutionContext): RedisChannel[A] = {
    val subscriptionThread = Executors.newSingleThreadExecutor(subscriberThreadFactory)
    apply(channelName, server, subscriptionThread, serializer, publishCtx)
  }

  def apply[A](channelName: String, server: JedisShardInfo, serializer: Serializer[A], publishCtx: concurrent.ExecutionContext): RedisChannel[A] = {
    apply(channelName, server, Threads.factory(classOf[RedisChannel[_]].getName), serializer, publishCtx)
  }

  def apply[A](channelName: String, server: JedisShardInfo, publishCtx: concurrent.ExecutionContext): RedisChannel[A] = {
    apply(channelName, server, new JavaSerializer[A], publishCtx)
  }

  def apply[A](channelName: String, server: JedisShardInfo, serializer: Serializer[A]): RedisChannel[A] = {
    apply(channelName, server, serializer, SameThreadExecution)
  }

  def apply[A](channelName: String, server: JedisShardInfo): RedisChannel[A] = {
    apply(channelName, server, new JavaSerializer[A], SameThreadExecution)
  }

}
