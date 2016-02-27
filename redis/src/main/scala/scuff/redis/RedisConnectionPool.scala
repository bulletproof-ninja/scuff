package scuff.redis

import java.util.concurrent.TimeUnit

import scala.concurrent.{ Future, Promise, TimeoutException }
import scala.concurrent.duration.{ Duration, FiniteDuration }
import scala.util.{ Failure, Success, Try }

import redis.clients.jedis.Jedis
import redis.clients.jedis.exceptions.JedisConnectionException
import redis.clients.util.Pool
import scuff.Clock
import scuff.concurrent._

class RedisConnectionPool(pool: Pool[Jedis], enforceDB: Option[Int]) extends CONNECTION {
  def apply(code: Jedis => Any): Any = connection(retry = true)(code)
  def this(pool: Pool[Jedis], enforceDB: Int) = this(pool, Some(enforceDB))
  private[this] val db = enforceDB.getOrElse(-1)
  def connection[T](retry: Boolean = true)(code: Jedis => T): T = {
    val jedis = pool.getResource()
    try {
      if (db != -1 && jedis.getDB != db) jedis.select(db)
      code(jedis)
    } catch {
      case je: JedisConnectionException =>
        if (retry) {
          connection(retry = false)(code)
        } else {
          throw je
        }
    } finally {
      jedis.close()
    }
  }

  /**
    * Execute code under a lock.
    * @param lockKey The key to lock. Make sure this key is globally unique, to avoid accidental name clash.
    * @param maxHoldLock The max time the key will be locked. It is ESSENTIAL that this time not be exceeded to guarantee exclusivity. It is advised to include a large margin of error.
    * @param maxWaitLock The max wait time allowed to acquire the lock. Passing 0 will force fail-fast behavior.
    * @param whenLocked Code block to execute when lock is acquired
    * @param clock Clock implementation. Defaults to system clock.
    */
  def lock[T](lockKey: String, maxHoldLock: FiniteDuration, maxWaitLock: Duration)(whenLocked: Jedis => T)(implicit clock: Clock = Clock.System): Future[T] = {
    val waitExpiry = if (maxWaitLock.isFinite) clock.now(TimeUnit.MILLISECONDS) + maxWaitLock.toMillis else Long.MaxValue
    lock(lockKey, maxHoldLock.toSeconds.toInt, waitExpiry, (maxWaitLock.isFinite && maxWaitLock.length == 0), whenLocked)
  }
  private def lock[T](lockKey: String, holdLockSeconds: Int, waitExpiry: Long, failFast: Boolean, whenLocked: Jedis => T)(implicit clock: Clock): Future[T] = {
    connection(retry = true) { jedis =>
      tryLock(lockKey, holdLockSeconds, waitExpiry, failFast, whenLocked, jedis)
    }
  }
  private def tryLock[T](lockKey: String, holdLockSeconds: Int, waitExpiry: Long, failFast: Boolean, whenLocked: Jedis => T, jedis: Jedis)(implicit clock: Clock): Future[T] = {
    val now = clock.now(TimeUnit.SECONDS)
    val thisExpiry = (now + holdLockSeconds).toInt
    if (jedis.setnx(lockKey, String.valueOf(thisExpiry)) == 1L) {
      locked(lockKey, holdLockSeconds, whenLocked, jedis)
    } else if (failFast) {
      Future failed new TimeoutException("Failed to immediately acquire lock")
    } else {
      jedis.get(lockKey) match {
        case null =>
          lock(lockKey, holdLockSeconds, waitExpiry, false, whenLocked)
        case thatExpiry =>
          val expiry = thatExpiry.toLong
          if (now >= expiry) {
            val replacedExpiry = jedis.getSet(lockKey, String.valueOf(thisExpiry))
            if (replacedExpiry == null || replacedExpiry == thatExpiry) {
              locked(lockKey, holdLockSeconds, whenLocked, jedis)
            } else {
              tryLockLater(lockKey, holdLockSeconds, waitExpiry, whenLocked)
            }
          } else {
            tryLockLater(lockKey, holdLockSeconds, waitExpiry, whenLocked)
          }
      }
    }
  }
  class LockTimeExceeded[T](val result: T, maxSeconds: Int) extends IllegalStateException(s"Lock time exceeded $maxSeconds seconds. Lock code was executed, but lock exclusivity cannot be guaranteed.")
  private def locked[T](lockKey: String, maxSeconds: Int, whenLocked: Jedis => T, jedis: Jedis)(implicit clock: Clock): Future[T] = {
    val redisExpiry = clock.now(TimeUnit.SECONDS) + maxSeconds
    try {
      jedis.expire(lockKey, maxSeconds)
      Try(whenLocked(jedis)) match {
        case Success(t) =>
          if (clock.now(TimeUnit.SECONDS) > redisExpiry) {
            Future failed new LockTimeExceeded(t, maxSeconds)
          } else {
            jedis.del(lockKey)
            Future successful t
          }
        case Failure(e) => Future failed e
      }
    } catch {
      case e: Exception => Future failed e
    }
  }
  private def tryLockLater[T](lockKey: String, maxSeconds: Int, waitExpiry: Long, whenLocked: Jedis => T)(implicit clock: Clock): Future[T] = {
    val promise = Promise[T]
    val retry = new Runnable {
      def run = promise completeWith lock(lockKey, maxSeconds, waitExpiry, false, whenLocked)
    }
    val now = clock.now(TimeUnit.MILLISECONDS)
    val delayMillis = {
      val delay = TimeUnit.SECONDS.toMillis(maxSeconds) / 250
      if (now + delay > waitExpiry) {
        waitExpiry - now
      } else {
        delay
      }
    }
    if (now + delayMillis >= waitExpiry) {
      promise.failure(new TimeoutException("Failed to acquire lock"))
    } else {
      Threads.DefaultScheduler.schedule(retry, delayMillis, TimeUnit.MILLISECONDS)
    }
    promise.future
  }

}
