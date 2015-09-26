package scuff.redis

import java.util.concurrent.{ Executor, Executors }
import java.util.concurrent.locks.ReentrantReadWriteLock
import redis.clients.jedis.{ BinaryJedis, BinaryJedisPubSub, JedisShardInfo }
import redis.clients.util.SafeEncoder
import scuff._
import scuff.concurrent._
import java.util.concurrent.ThreadFactory
import scala.concurrent.ExecutionContext

/**
  * Redis pub/sub channel.
  */
class BinaryRedisFaucet[A] private[redis] (
  channelName: Array[Byte],
  info: JedisShardInfo,
  subscriberThread: Executor,
  serializer: Serializer[A],
  publishCtx: ExecutionContext)
    extends Faucet {

  type Filter = A
  type Consumer = A => Unit

  private val (lockShared, lockExclusive, newSubscriber) = {
    val rwLock = new ReentrantReadWriteLock
    val exclusive = rwLock.writeLock
    (rwLock.readLock, exclusive, exclusive.newCondition)
  }
  private class FilteredSubscriber(sub: Consumer, doTell: A => Boolean) {
    def tell(a: A) =
      if (doTell(a)) {
        publishCtx execute new Runnable {
          def run = sub(a)
        }
      }
  }
  private[this] val subscribers = collection.mutable.Buffer[FilteredSubscriber]()

  private[this] val jedisSubscriber = new BinaryJedisPubSub {
    override def onMessage(channel: Array[Byte], byteMsg: Array[Byte]) {
      val msg = serializer.decode(byteMsg)
      lockShared {
        subscribers.foreach(_.tell(msg))
      }
    }
  }

  private[this] val jedis = new BinaryJedis(info)
  def subscribe(subscriber: Consumer, filter: A => Boolean) = {
    val filteredSub = new FilteredSubscriber(subscriber, filter)
    lockExclusive {
      newSubscriber.signalIf(subscribers.isEmpty)
      subscribers += filteredSub
    }
    new Subscription {
      def cancel() = lockExclusive {
        subscribers -= filteredSub
        if (subscribers.isEmpty) {
          jedisSubscriber.unsubscribe()
        }
      }
    }
  }

  private[redis] def start() {
    subscriberThread execute new Runnable {
      def run = try {
        while (!Thread.currentThread.isInterrupted) {
          lockExclusive {
            newSubscriber.await(subscribers.nonEmpty)
          }
          consumeMessages()
        }
      } catch {
        case _: InterruptedException => Thread.currentThread().interrupt()
        case e: Exception => publishCtx.reportFailure(e)
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

object BinaryRedisFaucet {

  /**
    * @param server Redis server information
    * @param jedisSubscriberThread Subscription thread.
    * This thread will be monopolized by Jedis, therefore,
    * the `Executor` should not be a fixed size thread-pool,
    * preferably not a thread-pool at all.
    * @param serializer The byte array decoder
    * @param publishCtx The execution context used to publish messages
    */
  def apply[A](
    channelName: String, server: JedisShardInfo, jedisSubscriberThread: Executor,
    serializer: Serializer[A], publishCtx: ExecutionContext): BinaryRedisFaucet[A] = {
    val rc = new BinaryRedisFaucet(SafeEncoder.encode(channelName), server, jedisSubscriberThread, serializer, publishCtx)
    rc.start()
    rc
  }

  def apply[A](channelName: String, server: JedisShardInfo, subscriberThreadFactory: java.util.concurrent.ThreadFactory,
               serializer: Serializer[A], publishCtx: ExecutionContext): BinaryRedisFaucet[A] = {
    val subscriptionThread = Executors newSingleThreadExecutor new ThreadFactory {
      def newThread(r: Runnable) = {
        val thread = subscriberThreadFactory.newThread(r)
        val threadName = {
          val name = thread.getName()
          if (name.contains(channelName)) name else s"$name[$channelName]"
        }
        thread.setName(threadName)
        thread
      }
    }
    apply(channelName, server, subscriptionThread, serializer, publishCtx)
  }

  def apply[A](channelName: String, server: JedisShardInfo, serializer: Serializer[A], publishCtx: ExecutionContext): BinaryRedisFaucet[A] = {
    apply(channelName, server, Threads.daemonFactory(channelName), serializer, publishCtx)
  }

  def apply[A](channelName: String, server: JedisShardInfo, publishCtx: ExecutionContext): BinaryRedisFaucet[A] = {
    apply(channelName, server, new JavaSerializer[A], publishCtx)
  }

}
