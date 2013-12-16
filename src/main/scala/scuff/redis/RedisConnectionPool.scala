package scuff.redis

import _root_.redis.clients.jedis._, exceptions._
import _root_.redis.clients.util.Pool

class RedisConnectionPool(pool: Pool[Jedis], enforceDB: Option[Int]) extends CONNECTION {
  def apply(code: Jedis => Any): Any = connection(code)
  def this(pool: Pool[Jedis], enforceDB: Int) = this(pool, Some(enforceDB))
  private[this] val db = enforceDB.getOrElse(-1)
  def connection[T](code: Jedis ⇒ T): T = {
    var returned = false
    val jedis = pool.getResource()
    try {
      if (db != -1 && jedis.getDB != db) jedis.select(db)
      code(jedis)
    } catch {
      case je: JedisConnectionException ⇒
        pool.returnBrokenResource(jedis: Jedis)
        returned = true
        throw je
    } finally {
      if (!returned) pool.returnResource(jedis)
    }
  }
  def pipeline[T](block: Pipeline => T): T = connection { conn => 
    val pl = conn.pipelined()
    val t = block(pl)
    pl.sync()
    t
  }
}