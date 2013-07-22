package scuff.redis

import _root_.redis.clients.jedis._, exceptions._
import _root_.redis.clients.util.Pool

class RedisConnectionPool(pool: Pool[Jedis], enforceDB: Option[Int]) {
  def this(pool: Pool[Jedis], enforceDB: Int) = this(pool, Some(enforceDB))
  private[this] val db = enforceDB.getOrElse(-1)
  def connection[T](block: Jedis ⇒ T): T = {
    var returned = false
    val jedis = pool.getResource()
    try {
      if (db != -1 && jedis.getDB != db) jedis.select(db)
      block(jedis)
    } catch {
      case je: JedisConnectionException ⇒
        pool.returnBrokenResource(jedis: Jedis)
        returned = true
        throw je
    } finally {
      if (!returned) pool.returnResource(jedis)
    }
  }
}