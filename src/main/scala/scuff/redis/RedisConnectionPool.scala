package scuff.redis

import _root_.redis.clients.jedis._
import _root_.redis.clients.util.Pool
import scuff.Clock
import scala.concurrent.Future
import java.util.concurrent.TimeUnit
import redis.clients.jedis.exceptions.JedisConnectionException
import scuff.Threads
import scala.concurrent.Promise

class RedisConnectionPool(pool: Pool[Jedis], enforceDB: Option[Int]) extends CONNECTION {
  def apply(code: Jedis ⇒ Any): Any = connection(retry = true)(code)
  def this(pool: Pool[Jedis], enforceDB: Int) = this(pool, Some(enforceDB))
  private[this] val db = enforceDB.getOrElse(-1)
  def connection[T](retry: Boolean = true)(code: Jedis ⇒ T): T = {
    var returned = false
    val jedis = pool.getResource()
    try {
      if (db != -1 && jedis.getDB != db) jedis.select(db)
      code(jedis)
    } catch {
      case je: JedisConnectionException ⇒
        pool.returnBrokenResource(jedis: Jedis)
        returned = true
        if (retry) {
          connection(retry = false)(code)
        } else {
          throw je
        }
    } finally {
      if (!returned) pool.returnResource(jedis)
    }
  }

  def pipeline[T](retry: Boolean = true)(block: Pipeline ⇒ T): T = connection(retry)(_.pipeline(block))

  def lock[T](lockKey: String, maxSeconds: Int, retry: Boolean = true)(whenLocked: Jedis ⇒ T)(implicit clock: Clock): Future[T] = {
    connection(retry) { jedis ⇒
      lock(lockKey, maxSeconds, whenLocked, jedis)
    }
  }

  private def locked[T](lockKey: String, maxSeconds: Int, whenLocked: Jedis ⇒ T, jedis: Jedis): Future[T] = try {
    jedis.expire(lockKey, maxSeconds)
    try {
      Future successful whenLocked(jedis)
    } finally {
      jedis.del(lockKey)
    }
  } catch {
    case e: Exception ⇒ Future failed e
  }

  private def tryLockLater[T](lockKey: String, maxSeconds: Int, whenLocked: Jedis ⇒ T)(implicit clock: Clock): Future[T] = {
    implicit val Seconds = TimeUnit.SECONDS
    val promise = Promise[T]
    val retry = new Runnable {
      def run = promise completeWith lock(lockKey, maxSeconds)(whenLocked)
    }
    val delayMillis = Seconds.toMillis(maxSeconds) / 250
    Threads.DefaultScheduler.schedule(retry, delayMillis, TimeUnit.MILLISECONDS)
    promise.future

  }
  private def lock[T](lockKey: String, maxSeconds: Int, whenLocked: Jedis ⇒ T, jedis: Jedis)(implicit clock: Clock): Future[T] = {
    implicit val Seconds = TimeUnit.SECONDS
    val myExpiry = String.valueOf(clock.now + maxSeconds)
    if (jedis.setnx(lockKey, myExpiry) == 1L) {
      locked(lockKey, maxSeconds, whenLocked, jedis)
    } else {
      jedis.get(lockKey) match {
        case null ⇒
          lock(lockKey, maxSeconds)(whenLocked)
        case otherExpiry ⇒
          val expiry = otherExpiry.toLong
          if (clock.now >= expiry) {
            val myExpiry = String.valueOf(clock.now + maxSeconds)
            val replacedExpiry = jedis.getSet(lockKey, myExpiry)
            if (replacedExpiry == null || replacedExpiry == otherExpiry) {
              locked(lockKey, maxSeconds, whenLocked, jedis)
            } else {
              tryLockLater(lockKey, maxSeconds, whenLocked)
            }
          } else {
            tryLockLater(lockKey, maxSeconds, whenLocked)
          }
      }
    }
  }

}