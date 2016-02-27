package scuff

import _root_.redis.clients.util.{ SafeEncoder, Pool }
import _root_.redis.clients.jedis._
import commands._
import language.implicitConversions
import scala.util.Try

/**
  * This package requires the Jedis project.
  */
package object redis {

  type JedisConnection = BinaryJedisCommands with JedisCommands with MultiKeyBinaryCommands with MultiKeyCommands
  type JedisPipeline = BinaryRedisPipeline with RedisPipeline with MultiKeyBinaryRedisPipeline

  type JedisMagnet = Either[JedisConnection, JedisPipeline]

  implicit def toJedisMagnet(jedis: JedisConnection): JedisMagnet = Left(jedis)
  implicit def toJedisMagnet(pl: JedisPipeline): JedisMagnet = Right(pl)

  implicit def uri2Info(uri: java.net.URI) = {
    val userInfo: Option[(String, Option[String])] = Option(uri.getUserInfo).map { userInfo =>
      userInfo.split(":") match {
        case Array(user) => user -> None
        case Array(user, pass) => user -> Some(pass)
      }
    }
    userInfo match {
      case Some((user, pass)) =>
        val info = uri.getPort match {
          case -1 => new JedisShardInfo(uri.getHost, user)
          case port => new JedisShardInfo(uri.getHost, port, user)
        }
        pass.foreach(info.setPassword)
        info
      case None =>
        uri.getPort match {
          case -1 => new JedisShardInfo(uri.getHost)
          case port => new JedisShardInfo(uri.getHost, port)
        }
    }
  }

  def newPool(info: JedisShardInfo, db: Option[Int], name: String = null, config: JedisPoolConfig = new JedisPoolConfig) = {
    if (config.getMaxWaitMillis == -1L) {
      config.setMaxWaitMillis(3333)
    }
    new JedisPool(config, info.getHost, info.getPort, Protocol.DEFAULT_TIMEOUT, info.getPassword, db.getOrElse(0), name)
  }

  implicit class ScuffJedis(private val jedis: BinaryJedis) extends AnyVal {
    /**
      * @return Some(value) if successful, None if any watches failed.
      */
    def transaction[T](block: Transaction => T): Option[T] = {
      val txn = jedis.multi()
      val t = try {
        block(txn)
      } catch {
        case e: Exception => Try(txn.discard); throw e
      }
      if (txn.exec() == null) {
        None
      } else {
        Some(t)
      }
    }
    /**
      * @return Value from transaction block
      * @throws IllegalStateException if a WATCH has been issued
      */
    @throws[IllegalStateException]
    def transactionNoWatch[T](block: Transaction => T): T = {
      if (jedis.getClient.isInWatch) throw new IllegalStateException("Connection is unexpectedly in WATCH mode")
      val txn = jedis.multi()
      val t = try {
        block(txn)
      } catch {
        case e: Exception => Try(txn.discard); throw e
      }
      txn.exec()
      t
    }

    def pipeline[T](block: Pipeline => T): T = {
      val pl = jedis.pipelined()
      val t = block(pl)
      if (jedis.isConnected) pl.sync()
      t
    }

  }

  type CONNECTION = (Jedis => Any) => Any
  /** Construct object by using a thread-safe connection pool. */
  //  def threadSafe[T](pool: RedisConnectionPool)(factory: CONNECTION => T): T = factory(block => pool.connection()(block))
  /** Construct object by using an on-demand connection. */
  def singleThreaded[T](config: JedisShardInfo, db: Int = 0)(factory: CONNECTION => T): T = {
    val jedis = new Jedis(config)
    try {
      jedis.select(db)
      factory(block => block(jedis))
    } finally {
      jedis.quit()
    }
  }
  /** Construct object by using a non-shared existing connection. */
  def singleThreaded[T](jedis: Jedis, db: Option[Int])(factory: CONNECTION => T): T = {
    db.foreach(jedis.select)
    factory(block => block(jedis))
  }
}
