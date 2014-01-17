package scuff

import _root_.redis.clients.jedis._
import _root_.redis.clients.util.Pool
import language.implicitConversions
import java.util.concurrent.TimeUnit
import scala.concurrent.Future

/**
 * This package requires the Jedis project.
 */
package object redis {
  implicit def uri2Info(uri: java.net.URI) = {
    val userInfo: Option[(String, Option[String])] = Option(uri.getUserInfo).map { userInfo ⇒
      userInfo.split(":") match {
        case Array(user) ⇒ user -> None
        case Array(user, pass) ⇒ user -> Some(pass)
      }
    }
    userInfo match {
      case Some((user, pass)) ⇒
        val info = uri.getPort match {
          case -1 ⇒ new JedisShardInfo(uri.getHost, user)
          case port ⇒ new JedisShardInfo(uri.getHost, port, user)
        }
        pass.foreach(info.setPassword)
        info
      case None ⇒
        uri.getPort match {
          case -1 ⇒ new JedisShardInfo(uri.getHost)
          case port ⇒ new JedisShardInfo(uri.getHost, port)
        }
    }
  }

  def newPool(info: JedisShardInfo, config: JedisPoolConfig = new JedisPoolConfig) = {
    new JedisPool(config, info.getHost, info.getPort, Protocol.DEFAULT_TIMEOUT, info.getPassword)
  }

  implicit final class ScuffJedis(val jedis: Jedis) extends AnyVal {
    /**
     * NOTICE: The transaction block may be executed multiple times,
     * so make sure it's idempotent.
     */
    def transaction[T](keys: String*)(block: Transaction ⇒ T): T = {
      if (keys.nonEmpty) jedis.watch(keys: _*)
      val txn = jedis.multi()
      try {
        val t = block(txn)
        if (txn.exec == null) {
          transaction(keys: _*)(block)
        } else {
          t
        }
      } catch {
        case e: Exception ⇒ try { txn.discard() } catch { case _: Exception ⇒ /* Ignore */ }; throw e
      }
    }
  }

  //  def atomicUpdate(jedis: Jedis, key: String)(updater: String ⇒ String) {
  //      def tryOptimistic(jedis: Jedis)(block: Transaction ⇒ Unit): Boolean = {
  //        val txn = jedis.multi()
  //        try {
  //          block(txn)
  //          txn.exec() != null
  //        } catch {
  //          case e: Exception ⇒ try { txn.discard() } catch { case _: Exception ⇒ /* Ignore */ }; throw e
  //        }
  //      }
  //    jedis.watch(key)
  //    val value = jedis.get(key)
  //  }

  type CONNECTION = (Jedis ⇒ Any) ⇒ Any
  /** Construct object by using a thread-safe connection pool. */
  def threadSafe[T](pool: RedisConnectionPool)(factory: CONNECTION ⇒ T): T = factory(block ⇒ pool.connection(block))
  /** Construct object by using an on-demand connection. */
  def singleThreaded[T](config: JedisShardInfo, db: Int = 0)(factory: CONNECTION ⇒ T): T = {
    val jedis = new Jedis(config)
    try {
      jedis.select(db)
      factory(block ⇒ block(jedis))
    } finally {
      jedis.disconnect()
    }
  }
  /** Construct object by using a non-shared existing connection. */
  def singleThreaded[T](jedis: Jedis, db: Option[Int])(factory: CONNECTION ⇒ T): T = {
    db.foreach(jedis.select)
    factory(block ⇒ block(jedis))
  }
}