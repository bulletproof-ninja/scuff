package scuff.redis

import _root_.redis.clients.jedis._, exceptions._
import _root_.redis.clients.util.Pool

class RedisConnectionPool(db: Int, pool: Pool[Jedis]) {
  def connection[T](block: Jedis ⇒ T): T = {
    var returned = false
    val jedis = pool.getResource()
    try {
      if (jedis.getDB != db) jedis.select(db)
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