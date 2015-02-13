package scuff.redis

import _root_.redis.clients.jedis._
import scala.concurrent.duration.FiniteDuration
import language.implicitConversions

class BinaryRedisCache[K, V](val defaultTTL: FiniteDuration, conn: CONNECTION, keySer: scuff.Serializer[K], valueSer: scuff.Serializer[V])
    extends scuff.Cache[K, V] with scuff.Expiry[K, V] {

  private implicit def durToSecs(d: FiniteDuration): Int = d.toSeconds.toFloat.round

  @volatile private[this] var isShutdown = false
  private def connection[T] = if (isShutdown) {
    throw new IllegalStateException("Cache has been shut down")
  } else {
    conn.asInstanceOf[(Jedis ⇒ T) ⇒ T]
  }

  def store(key: K, value: V, ttl: FiniteDuration) =
    if (ttl.length > 0) {
      connection(_.setex(keySer.encode(key), ttl, valueSer.encode(value)))
    } else {
      connection(_.set(keySer.encode(key), valueSer.encode(value)))
    }

  def evict(key: K): Boolean = {
    connection(_.del(keySer encode key) != 0L)
  }

  def lookupAndEvict(key: K): Option[V] = {
    val keyBytes: Array[Byte] = keySer.encode(key)
    val removed = atomic { txn ⇒
      val removed = txn.get(keyBytes)
      (txn: BinaryRedisPipeline).del(keyBytes: Array[Byte])
      removed
    }
    Option(removed.get).map(valueSer.decode)
  }

  def lookup(key: K): Option[V] = Option(getOrNull(key))

  def lookupAndRefresh(key: K, ttl: FiniteDuration): Option[V] = connection { jedis ⇒
    getOrNull(keySer.encode(key), jedis) match {
      case null ⇒ None
      case value ⇒
        refresh(key, ttl, jedis)
        Some(value)
    }
  }

  private def refresh(key: K, ttl: FiniteDuration, jedis: Jedis): Boolean = jedis.expire(keySer.encode(key), ttl) == 1L

  def refresh(key: K, ttl: FiniteDuration): Boolean = connection(refresh(key, ttl, _))

  def contains(key: K): Boolean = connection(_.exists(keySer encode key))

  private def getOrNull(key: K): V = connection(getOrNull(keySer.encode(key), _))
  private def getOrNull(keyBytes: Array[Byte], jedis: Jedis): V =
    jedis.get(keyBytes) match {
      case null ⇒ null.asInstanceOf[V]
      case bytes ⇒ valueSer.decode(bytes)
    }
  def lookupOrStore(key: K, ttl: FiniteDuration)(constructor: ⇒ V): V = connection(lookupOrStore(_, keySer.encode(key), ttl, constructor))
  private def lookupOrStore(jedis: Jedis, keyBytes: Array[Byte], ttl: FiniteDuration, constructor: ⇒ V): V = {
    val value = getOrNull(keyBytes, jedis)
    if (value != null) {
      value
    } else {
      val value = constructor
      val success = jedis.setnx(keyBytes, valueSer.encode(value)) == 1L
      if (success) {
        if (ttl.length > 0) {
          jedis.expire(keyBytes, ttl)
        }
        value
      } else {
        // That was racy. Try again
        lookupOrStore(jedis, keyBytes, ttl, constructor)
      }
    }
  }

  def shutdown() {
    connection(_.flushDB)
    isShutdown = true
  }

  import collection.JavaConverters._

  def set(key: K, value: V) = connection(_.set(keySer.encode(key), valueSer.encode(value)))

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
