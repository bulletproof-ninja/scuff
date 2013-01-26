package scuff

import _root_.redis.clients.jedis._
import _root_.redis.clients.util.Pool

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

  type CONNECTION = (Jedis ⇒ Any) ⇒ Any
  def threadSafe[T](pool: RedisConnectionPool)(factory: CONNECTION ⇒ T): T = factory(block ⇒ pool.connection(block))
  def singleThreaded[T](config: JedisShardInfo, db: Int = 0)(factory: CONNECTION ⇒ T): T = {
    val jedis = new Jedis(config)
    jedis.select(db)
    factory(block ⇒ block(jedis))
  }
  def singleThreaded[T](jedis: Jedis, db: Option[Int])(factory: CONNECTION ⇒ T): T = {
    db.foreach(jedis.select)
    factory(block ⇒ block(jedis))
  }
}