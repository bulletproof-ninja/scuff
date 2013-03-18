package scuff.redis

import redis.clients.jedis._
import redis.clients.util.SafeEncoder._

/**
 * Redis implementation of [[scala.collection.mutable.ConcurrentMap]].
 * NOTICE: This implementation does not support the CAS versions of `remove` and 
 * `replace` due to inefficiencies such implementation would lead to.
 * See [[scuff.redis.RedisMap]] if such functionality is needed.
 */
class RedisHashMap[K, V](name: String, conn: CONNECTION, keySer: scuff.Serializer[K], valueSer: scuff.Serializer[V])
    extends collection.concurrent.Map[K, V] {
  import collection.JavaConverters._

  private def connection[T] = conn.asInstanceOf[(Jedis ⇒ T) ⇒ T]

  private[this] val hkey = encode(name)

  def set(field: K, value: V) = connection(_.hset(hkey, keySer.forth(field), valueSer.forth(value)))

  private def atomic[T](block: Transaction ⇒ T): T = {
    connection { jedis ⇒
      val txn = jedis.multi()
      try {
        val t = block(txn)
        txn.exec()
        t
      } catch {
        case e: Exception ⇒ try { txn.discard() } catch { case _: Exception ⇒ /* Ignore */ }; throw e
      }
    }
  }

  override def contains(field: K): Boolean = connection(_.hexists(hkey, keySer.forth(field)))

  override def keySet(): Set[K] = keys.toSet
  override def keys(): Iterable[K] = {
    val keys = connection(_.hkeys(hkey))
    keys.asScala.view.map(keySer.back)
  }
  override def values(): Iterable[V] = {
    val values = connection(_.hvals(hkey))
    values.asScala.view.map(valueSer.back)
  }

  override def put(field: K, value: V): Option[V] = {
    val hfield = keySer.forth(field)
    val prev = atomic { txn ⇒
      val prev = txn.hget(hkey, hfield)
      txn.hset(hkey, hfield, valueSer.forth(value))
      prev
    }
    Option(prev.get).map(valueSer.back)
  }
  override def remove(field: K): Option[V] = {
    val hfield = keySer.forth(field)
    val removed = atomic { txn ⇒
      val removed = txn.hget(hkey, hfield)
      txn.hdel(hkey, hfield)
      removed
    }
    Option(removed.get).map(valueSer.back)
  }
  def -=(field: K): this.type = {
    connection(_.hdel(hkey, keySer.forth(field)))
    this
  }
  def +=(entry: (K, V)): this.type = {
    connection(_.hset(hkey, keySer.forth(entry._1), valueSer.forth(entry._2)))
    this
  }
  def get(field: K): Option[V] = Option(getOrNull(field))
  override def apply(field: K): V = {
    getOrNull(field) match {
      case null ⇒ throw new NoSuchElementException("Not found: " + field)
      case value ⇒ value
    }
  }
  private def getOrNull(field: K): V = {
    connection { jedis ⇒
      jedis.hget(hkey, keySer.forth(field)) match {
        case null ⇒ null.asInstanceOf[V]
        case bytes ⇒ valueSer.back(bytes)
      }
    }
  }
  def setIfAbsent(field: K, value: V): Boolean = connection(_.hsetnx(hkey, keySer.forth(field), valueSer.forth(value)) == 1L)

  def putIfAbsent(field: K, value: V): Option[V] = {
    val hfield = keySer.forth(field)
    val (prevValResp, successResp) = atomic { txn ⇒
      txn.hget(hkey, hfield) -> txn.hsetnx(hkey, hfield, valueSer.forth(value))
    }
    if (successResp.get == 1L) {
      None
    } else {
      Option(prevValResp.get).map(valueSer.back)
    }
  }

  def del(field: K): Boolean = connection(_.hdel(hkey, keySer.forth(field)) == 1L)

  def iterator(): Iterator[(K, V)] = {
    val mapped = connection(_.hgetAll(hkey)).entrySet.asScala.view.map { entry ⇒
      keySer.back(entry.getKey) -> valueSer.back(entry.getValue)
    }
    mapped.iterator
  }

  // We could implement this using WATCH, but since it's too wide in scope, there's the potential of far too many false positives. Subclass if needed.
  def remove(field: K, expectedValue: V): Boolean = throw new UnsupportedOperationException("Redis does not support CAS operations on hash entries")
  def replace(field: K, newValue: V): Option[V] = throw new UnsupportedOperationException("Redis does not support CAS operations on hash entries")
  def replace(field: K, oldValue: V, newValue: V): Boolean = throw new UnsupportedOperationException("Redis does not support CAS operations on hash entries")

  override def clear() = conn(_.del(name))
}