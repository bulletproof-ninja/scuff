package scuff.redis

import _root_.redis.clients.jedis._
import _root_.redis.clients.util._

/**
 * Redis Hash implementation of [[scala.collection.mutable.ConcurrentMap]].
 * NOTICE: This implementation does not support the CAS versions of `remove` and
 * `replace` due to inefficiencies such implementation would lead to.
 * @see [[scuff.redis.RedisMap]] if such functionality is needed.
 */
class BinaryRedisHashMap[K, V](name: String, conn: CONNECTION, keySer: scuff.Serializer[K], valueSer: scuff.Serializer[V])
    extends collection.concurrent.Map[K, V] {
  import collection.JavaConverters._

  implicit private def connection[T] = conn.asInstanceOf[(Jedis => T) => T]

  private[this] val hkey = SafeEncoder.encode(name)

  def set(field: K, value: V) = connection(_.hset(hkey, keySer.encode(field), valueSer.encode(value)))

  override def contains(field: K): Boolean = connection(_.hexists(hkey, keySer.encode(field)))

  override def keySet: Set[K] = keys.toSet
  override def keys: Iterable[K] = {
    val keys = connection(_.hkeys(hkey))
    keys.asScala.view.map(keySer.decode)
  }
  override def values: Iterable[V] = {
    val values = connection(_.hvals(hkey))
    values.asScala.view.map(valueSer.decode)
  }

  override def put(field: K, value: V): Option[V] = {
    val hfield = keySer.encode(field)
    val prev = connection { conn =>
      conn.transactionNoWatch { txn =>
        val prev = txn.hget(hkey, hfield)
        txn.hset(hkey, hfield, valueSer.encode(value))
        prev
      }
    }
    Option(prev.get).map(valueSer.decode)
  }
  override def remove(field: K): Option[V] = {
    val hfield = keySer.encode(field)
    val removed = connection { conn =>
      conn.transactionNoWatch { txn =>
        val removed = txn.hget(hkey, hfield)
        txn.hdel(hkey, hfield)
        removed
      }
    }
    Option(removed.get).map(valueSer.decode)
  }
  def -=(field: K): this.type = {
    connection(_.hdel(hkey, keySer.encode(field)))
    this
  }
  def +=(entry: (K, V)): this.type = {
    connection(_.hset(hkey, keySer.encode(entry._1), valueSer.encode(entry._2)))
    this
  }
  def get(field: K): Option[V] = Option(getOrNull(field))
  override def apply(field: K): V = {
    getOrNull(field) match {
      case null => throw new NoSuchElementException("Not found: " + field)
      case value => value
    }
  }
  private def getOrNull(field: K): V = {
    connection { jedis =>
      jedis.hget(hkey, keySer.encode(field)) match {
        case null => null.asInstanceOf[V]
        case bytes => valueSer.decode(bytes)
      }
    }
  }
  def setIfAbsent(field: K, value: V): Boolean = connection(_.hsetnx(hkey, keySer.encode(field), valueSer.encode(value)) == 1L)

  def putIfAbsent(field: K, value: V): Option[V] = {
    val hfield = keySer.encode(field)
    val (prevValResp, successResp) = connection { conn =>
      conn.transactionNoWatch { txn =>
        txn.hget(hkey, hfield) -> txn.hsetnx(hkey, hfield, valueSer.encode(value))
      }
    }
    if (successResp.get == 1L) {
      None
    } else {
      Option(prevValResp.get).map(valueSer.decode)
    }
  }

  def del(field: K): Boolean = connection(_.hdel(hkey, keySer.encode(field)) == 1L)

  def iterator: Iterator[(K, V)] = {
    val mapped = connection(_.hgetAll(hkey)).entrySet.asScala.view.map { entry =>
      keySer.decode(entry.getKey) -> valueSer.decode(entry.getValue)
    }
    mapped.iterator
  }

  // We could implement this using WATCH, but since it's too wide in scope, there's the potential of far too many false positives. Subclass if needed.
  @deprecated("Unsupported", since="Forever")
  def remove(field: K, expectedValue: V): Boolean = throw new UnsupportedOperationException("Redis does not support CAS operations on hash entries")
  @deprecated("Unsupported", since="Forever")
  def replace(field: K, newValue: V): Option[V] = throw new UnsupportedOperationException("Redis does not support CAS operations on hash entries")
  @deprecated("Unsupported", since="Forever")
  def replace(field: K, oldValue: V, newValue: V): Boolean = throw new UnsupportedOperationException("Redis does not support CAS operations on hash entries")

  override def clear() = conn(_.del(name))
}
