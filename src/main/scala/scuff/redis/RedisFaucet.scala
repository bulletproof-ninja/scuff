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
class RedisFaucet private (
  channelName: String,
  info: JedisShardInfo,
  subscriberThread: Executor,
  publishCtx: concurrent.ExecutionContext)
    extends BinaryRedisFaucet(SafeEncoder.encode(channelName), info, subscriberThread, StringSerializer, publishCtx) {
}

object RedisFaucet {

  /**
   * @param server Redis server information
   * @param jedisSubscriberThread Subscription thread.
   * This thread will be monopolized by Jedis, therefore,
   * the `Executor` should NOT be a general purpose thread-pool.
   * @param publishCtx The execution context used to publish messages
   */
  def apply(
    channelName: String,
    server: JedisShardInfo,
    jedisSubscriberThread: Executor,
    publishCtx: concurrent.ExecutionContext): RedisFaucet = {

    val rc = new RedisFaucet(channelName, server, jedisSubscriberThread, publishCtx)
    rc.start()
    rc
  }

  def apply(
    channelName: String,
    server: JedisShardInfo,
    subscriberThreadFactory: java.util.concurrent.ThreadFactory,
    publishCtx: concurrent.ExecutionContext): RedisFaucet = {
    val subscriptionThread = Threads.newSingleRunExecutor(subscriberThreadFactory)
    apply(channelName, server, subscriptionThread, publishCtx)
  }

  def apply(channelName: String, server: JedisShardInfo, publishCtx: concurrent.ExecutionContext): RedisFaucet = {
    apply(channelName, server, Threads.factory(classOf[RedisFaucet].getName), publishCtx)
  }

}
