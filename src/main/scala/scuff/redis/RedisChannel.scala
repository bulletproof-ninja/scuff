package scuff.redis

import redis.clients.jedis._
import redis.clients.util._
import java.util.concurrent._
import collection.JavaConverters._
import java.util.concurrent.locks._

/**
 * NOTICE: This class will monopolize
 * a single thread from the given `Executor`.
 * But only one, regardless of number of
 * subscribers.
 */
class RedisChannel[A](
  channelName: String,
  info: JedisShardInfo,
  subscriberThread: Executor,
  errorHandler: Exception ⇒ Unit,
  serializer: scuff.Serializer[A])
    extends scuff.Channel {

  def this(
    channelName: String,
    info: JedisShardInfo,
    subscriberThreadFactory: ThreadFactory,
    errorHandler: Exception ⇒ Unit,
    serializer: scuff.Serializer[A]) =
    this(channelName, info, Executors.newSingleThreadExecutor(subscriberThreadFactory), errorHandler, serializer)

  def this(
    channelName: String,
    info: JedisShardInfo,
    errorHandler: Exception ⇒ Unit = (e) ⇒ e.printStackTrace(System.err),
    serializer: scuff.Serializer[A] = new scuff.JavaSerializer[A]) =
    this(channelName, info, scuff.ThreadFactory(classOf[RedisChannel[_]].getName), errorHandler, serializer)

  type F = A
  type L = A ⇒ Unit
  private[this] val byteName = SafeEncoder.encode(channelName)

  private val (shared, exclusive, newSubscriber) = {
    val rwLock = new ReentrantReadWriteLock
    val exclusive = rwLock.writeLock
    (rwLock.readLock, exclusive, exclusive.newCondition)
  }
  private def whenLocked[T](l: Lock)(f: ⇒ T): T = {
    l.lock()
    try { f } finally {
      l.unlock()
    }
  }
  private class FilteredSubscriber(sub: L, allow: F ⇒ Boolean) {
    def apply(a: A) = if (allow(a)) sub(a)
  }
  private val subscribers = collection.mutable.Buffer[FilteredSubscriber]()

  private val jedisSubscriber = new BinaryJedisPubSub {
    def onMessage(channel: Array[Byte], msg: Array[Byte]) {
      val aMsg = serializer.back(msg)
      whenLocked(shared) {
        subscribers.foreach { sub ⇒
          try { sub(aMsg) } catch {
            case e: Exception ⇒ errorHandler(e)
          }
        }
      }
    }
    def onPMessage(pattern: Array[Byte], channel: Array[Byte], msg: Array[Byte]) {}
    def onPSubscribe(channel: Array[Byte], noSubs: Int) {}
    def onPUnsubscribe(channel: Array[Byte], noSubs: Int) {}
    def onSubscribe(channel: Array[Byte], noSubs: Int) {}
    def onUnsubscribe(channel: Array[Byte], noSubs: Int) {}
  }

  private[this] val jedis = new BinaryJedis(info)
  subscriberThread execute new Runnable {
    def run = while (!Thread.currentThread.isInterrupted) try {
      awaitSubscribers()
      consumeMessages()
    } catch {
      case _: InterruptedException ⇒ Thread.currentThread().interrupt()
      case e: Exception ⇒ errorHandler(e)
    }

    def awaitSubscribers() = whenLocked(exclusive) {
      while (subscribers.isEmpty) {
        newSubscriber.await()
      }
    }

    def consumeMessages() = try {
      jedis.connect()
      jedis.subscribe(jedisSubscriber, byteName)
    } finally {
      jedis.disconnect()
    }
  }
  def subscribe(subscriber: L, filter: A ⇒ Boolean) = {
    val filteredSub = new FilteredSubscriber(subscriber, filter)
    whenLocked(exclusive) {
      if (subscribers.isEmpty) {
        newSubscriber.signal()
      }
      subscribers += filteredSub
    }
    new scuff.Subscription {
      def cancel() = whenLocked(exclusive) {
        subscribers -= filteredSub
        if (subscribers.isEmpty) {
          jedisSubscriber.unsubscribe()
        }
      }
    }
  }
}
