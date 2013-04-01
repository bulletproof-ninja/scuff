package scuff.redis

import redis.clients.jedis._
import redis.clients.util.SafeEncoder._

class RedisCache[K, V](val defaultTTL: Int, conn: CONNECTION, keySer: scuff.Serializer[K], valueSer: scuff.Serializer[V])
    extends scuff.Cache[K, V] {

  @volatile private[this] var disabled = false
  private def connection[T] = if (disabled) {
    throw new IllegalStateException("Cache has been disabled")
  } else {
    conn.asInstanceOf[(Jedis ⇒ T) ⇒ T]
  }

  def store(key: K, value: V, ttlSeconds: Int) =
    if (ttlSeconds > 0) {
      connection(_.setex(keySer.forth(key), ttlSeconds, valueSer.forth(value)))
    } else {
      connection(_.set(keySer.forth(key), valueSer.forth(value)))
    }

  def evict(key: K): Option[V] = {
    val keyBytes = keySer.forth(key)
    val removed = atomic { txn ⇒
      val removed = txn.get(keyBytes)
      txn.del(keyBytes)
      removed
    }
    Option(removed.get).map(valueSer.back)
  }

  def lookup(key: K): Option[V] = Option(getOrNull(key))

  def lookupAndRefresh(key: K, ttl: Int): Option[V] = connection { jedis ⇒
    getOrNull(keySer.forth(key), jedis) match {
      case null ⇒ None
      case value ⇒
        refresh(key, ttl, jedis)
        Some(value)
    }
  }

  private def refresh(key: K, ttl: Int, jedis: Jedis): Boolean = jedis.expire(keySer.forth(key), ttl) == 1L

  def refresh(key: K, ttl: Int): Boolean = connection(refresh(key, ttl, _))

  private def getOrNull(key: K): V = connection(getOrNull(keySer.forth(key), _))
  private def getOrNull(keyBytes: Array[Byte], jedis: Jedis): V =
    jedis.get(keyBytes) match {
      case null ⇒ null.asInstanceOf[V]
      case bytes ⇒ valueSer.back(bytes)
    }
  def lookupOrStore(key: K, ttlSeconds: Int)(constructor: ⇒ V): V = connection(lookupOrStore(_, keySer.forth(key), ttlSeconds, constructor))
  private def lookupOrStore(jedis: Jedis, keyBytes: Array[Byte], ttlSeconds: Int, constructor: ⇒ V): V = {
    val value = getOrNull(keyBytes, jedis)
    if (value != null) {
      value
    } else {
      val value = constructor
      val (prevValResp, successResp) = atomic(jedis) { txn ⇒
        txn.get(keyBytes) -> txn.setnx(keyBytes, valueSer.forth(value))
      }
      if (successResp.get == 1L) {
        if (ttlSeconds > 0) {
          jedis.expire(keyBytes, ttlSeconds)
        }
        value
      } else {
        // That was racy. Try again
        lookupOrStore(jedis, keyBytes, ttlSeconds, constructor)
      }
    }
  }

  def disable() = connection(_.flushDB); disabled = true

  import collection.JavaConverters._

  def set(key: K, value: V) = connection(_.set(keySer.forth(key), valueSer.forth(value)))

  private def atomic[T](jedis: Jedis)(block: Transaction ⇒ T): T = {
    val txn = jedis.multi()
    try {
      val t = block(txn)
      txn.exec()
      t
    } catch {
      case e: Exception ⇒ try { txn.discard() } catch { case _: Exception ⇒ /* Ignore */ }; throw e
    }
  }

  private def atomic[T](block: Transaction ⇒ T): T = connection(jedis ⇒ atomic(jedis)(block))

}