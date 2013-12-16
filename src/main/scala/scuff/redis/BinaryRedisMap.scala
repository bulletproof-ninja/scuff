package scuff.redis

import redis.clients.jedis._
import redis.clients.util.SafeEncoder._

/**
 * Redis general implementation of [[scala.collection.mutable.ConcurrentMap]].
 * @see [[scuff.redis.RedisHashMap]] for Hash implementation.
 */
class BinaryRedisMap[K, V](conn: CONNECTION, keySer: scuff.Serializer[K], valueSer: scuff.Serializer[V])
    extends collection.concurrent.Map[K, V] {
  import collection.JavaConverters._

  implicit private def connection[T] = conn.asInstanceOf[(Jedis ⇒ T) ⇒ T]

  def set(key: K, value: V) = connection(_.set(keySer.encode(key), valueSer.encode(value)))

  private def watch[T](jedis: Jedis, key: Array[Byte], proceed: V ⇒ Boolean, block: Transaction ⇒ Unit): Option[V] = {
      def tryOptimistic(jedis: Jedis)(block: Transaction ⇒ Unit): Boolean = {
        val txn = jedis.multi()
        try {
          block(txn)
          txn.exec() != null
        } catch {
          case e: Exception ⇒ try { txn.discard() } catch { case _: Exception ⇒ /* Ignore */ }; throw e
        }
      }
    jedis.watch(key)
    Option(jedis.get(key)).map(valueSer.decode) match {
      case someValue @ Some(value) if proceed(value) ⇒
        tryOptimistic(jedis)(block) match {
          case true ⇒ someValue
          case false ⇒ watch(jedis, key, proceed, block)
        }
      case _ ⇒
        jedis.unwatch()
        None
    }
  }

  private def watch(key: Array[Byte])(proceed: V ⇒ Boolean)(block: Transaction ⇒ Unit): Option[V] = {
    connection(jedis ⇒ watch(jedis, key, proceed, block))
  }

  override def contains(key: K): Boolean = connection(_.exists(keySer.encode(key)))

  private[this] final val ALL = encode("*")

  override def keySet(): Set[K] = keys.toSet
  override def keys(): Iterable[K] = connection(_.keys(ALL)).asScala.view.map(keySer.decode)

  override def put(key: K, value: V): Option[V] = Option(connection(_.getSet(keySer.encode(key), valueSer.encode(value)))).map(valueSer.decode)

  override def remove(key: K): Option[V] = {
    val keyBytes = keySer.encode(key)
    val removed = transaction { txn ⇒
      val removed = txn.get(keyBytes)
      txn.del(keyBytes)
      removed
    }
    Option(removed.get).map(valueSer.decode)
  }
  def -=(key: K): this.type = {
    connection(_.del(keySer.encode(key)))
    this
  }
  def +=(entry: (K, V)): this.type = {
    connection(_.set(keySer.encode(entry._1), valueSer.encode(entry._2)))
    this
  }
  def get(key: K): Option[V] = Option(getOrNull(key))
  override def apply(key: K): V = {
    getOrNull(key) match {
      case null ⇒ throw new NoSuchElementException("Not found: " + key)
      case value ⇒ value
    }
  }
  private def getOrNull(key: K): V = {
    connection { jedis ⇒
      jedis.get(keySer.encode(key)) match {
        case null ⇒ null.asInstanceOf[V]
        case bytes ⇒ valueSer.decode(bytes)
      }
    }
  }
  def setIfAbsent(key: K, value: V): Boolean = connection(_.setnx(keySer.encode(key), valueSer.encode(value)) == 1L)

  def putIfAbsent(key: K, value: V): Option[V] = {
    val keyBytes = keySer.encode(key)
    val (prevValResp, successResp) = transaction { txn ⇒
      txn.get(keyBytes) -> txn.setnx(keyBytes, valueSer.encode(value))
    }
    if (successResp.get == 1L) {
      None
    } else {
      Option(prevValResp.get).map(valueSer.decode)
    }
  }

  def del(key: K): Boolean = connection(_.del(keySer.encode(key)) == 1L)

  override def clear() = connection(_.flushDB())

  def iterator(): Iterator[(K, V)] = {
    connection { jedis ⇒
      val keys = jedis.keys(ALL).asScala.toIterable
      keys.map { key ⇒
        keySer.decode(key) -> valueSer.decode(jedis.get(key))
      }
    }.iterator
  }

  def remove(key: K, expectedValue: V): Boolean = {
    val keyBytes = keySer.encode(key)
    val result = watch(keyBytes)(expectedValue == _) { txn ⇒
      txn.del(keyBytes)
    }
    result.isDefined
  }

  def replace(key: K, newValue: V): Option[V] = {
    val keyBytes = keySer.encode(key)
    val result = watch(keyBytes)(old ⇒ true) { txn ⇒
      txn.set(keyBytes, valueSer.encode(newValue))
    }
    result
  }
  def replace(key: K, oldValue: V, newValue: V): Boolean = {
    val keyBytes = keySer.encode(key)
    val result = watch(keyBytes)(oldValue == _) { txn ⇒
      txn.set(keyBytes, valueSer.encode(newValue))
    }
    result.isDefined
  }

}
