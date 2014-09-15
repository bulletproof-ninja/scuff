package scuff

import _root_.redis.clients.util.SafeEncoder
import _root_.redis.clients.jedis._
import _root_.redis.clients.util.Pool
import language.implicitConversions
import scala.util.Try

/**
 * This package requires the Jedis project.
 */
package object redis {

  implicit def toLeftJedis(jedis: BinaryJedis): Either[BinaryJedis, Pipeline] = Left(jedis)
  implicit def toRightPipeline(pl: Pipeline): Either[BinaryJedis, Pipeline] = Right(pl)

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

  def newPool(info: JedisShardInfo, db: Option[Int], name: String = null, config: JedisPoolConfig = new JedisPoolConfig) = {
    new JedisPool(config, info.getHost, info.getPort, Protocol.DEFAULT_TIMEOUT, info.getPassword, db.getOrElse(0), name)
  }

  implicit final class ScuffJedis(val jedis: BinaryJedis) extends AnyVal {
    /**
     * NOTICE: The transaction block may be executed multiple times,
     * so make sure it's idempotent.
     */
    def transaction[T](watch: String*)(block: Transaction ⇒ T): T = {
      if (watch.nonEmpty) jedis.watch(watch.map(SafeEncoder.encode): _*)
      val txn = jedis.multi()
      val t = try {
        block(txn)
      } catch {
        case e: Exception ⇒ Try(txn.discard); throw e
      }
      if (txn.exec() == null) {
        transaction(watch: _*)(block)
      } else {
        t
      }
    }

    def pipeline[T](block: Pipeline ⇒ T): T = {
      val pl = jedis.pipelined()
      val t = block(pl)
      pl.sync()
      t
    }

  }

  type CONNECTION = (Jedis ⇒ Any) ⇒ Any
  /** Construct object by using a thread-safe connection pool. */
  //  def threadSafe[T](pool: RedisConnectionPool)(factory: CONNECTION ⇒ T): T = factory(block ⇒ pool.connection()(block))
  /** Construct object by using an on-demand connection. */
  def singleThreaded[T](config: JedisShardInfo, db: Int = 0)(factory: CONNECTION ⇒ T): T = {
    val jedis = new Jedis(config)
    try {
      jedis.select(db)
      factory(block ⇒ block(jedis))
    } finally {
      jedis.quit()
    }
  }
  /** Construct object by using a non-shared existing connection. */
  def singleThreaded[T](jedis: Jedis, db: Option[Int])(factory: CONNECTION ⇒ T): T = {
    db.foreach(jedis.select)
    factory(block ⇒ block(jedis))
  }
}