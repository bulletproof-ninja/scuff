package scuff.redis

import redis.clients.jedis._
import redis.clients.util.SafeEncoder._

class RedisMap[K, V](conn: CONNECTION, keySer: scuff.Serializer[K], valueSer: scuff.Serializer[V])
    extends collection.concurrent.Map[K, V] {
  import collection.JavaConverters._

  implicit private def connection[T] = conn.asInstanceOf[(Jedis ⇒ T) ⇒ T]

  def set(key: K, value: V) = connection(_.set(keySer.forth(key), valueSer.forth(value)))

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
    Option(jedis.get(key)).map(valueSer.back) match {
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

  override def contains(key: K): Boolean = connection(_.exists(keySer.forth(key)))

  private[this] final val ALL = encode("*")

  override def keySet(): Set[K] = keys.toSet
  override def keys(): Iterable[K] = connection(_.keys(ALL)).asScala.view.map(keySer.back)

  override def put(key: K, value: V): Option[V] = Option(connection(_.getSet(keySer.forth(key), valueSer.forth(value)))).map(valueSer.back)

  override def remove(key: K): Option[V] = {
    val keyBytes = keySer.forth(key)
    val removed = transaction { txn ⇒
      val removed = txn.get(keyBytes)
      txn.del(keyBytes)
      removed
    }
    Option(removed.get).map(valueSer.back)
  }
  def -=(key: K): this.type = {
    connection(_.del(keySer.forth(key)))
    this
  }
  def +=(entry: (K, V)): this.type = {
    connection(_.set(keySer.forth(entry._1), valueSer.forth(entry._2)))
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
      jedis.get(keySer.forth(key)) match {
        case null ⇒ null.asInstanceOf[V]
        case bytes ⇒ valueSer.back(bytes)
      }
    }
  }
  def setIfAbsent(key: K, value: V): Boolean = connection(_.setnx(keySer.forth(key), valueSer.forth(value)) == 1L)

  def putIfAbsent(key: K, value: V): Option[V] = {
    val keyBytes = keySer.forth(key)
    val (prevValResp, successResp) = transaction { txn ⇒
      txn.get(keyBytes) -> txn.setnx(keyBytes, valueSer.forth(value))
    }
    if (successResp.get == 1L) {
      None
    } else {
      Option(prevValResp.get).map(valueSer.back)
    }
  }

  def del(key: K): Boolean = connection(_.del(keySer.forth(key)) == 1L)

  override def clear() = connection(_.flushDB())

  def iterator(): Iterator[(K, V)] = {
    connection { jedis ⇒
      val keys = jedis.keys(ALL).asScala.toIterable
      keys.map { key ⇒
        keySer.back(key) -> valueSer.back(jedis.get(key))
      }
    }.iterator
  }

  def remove(key: K, expectedValue: V): Boolean = {
    val keyBytes = keySer.forth(key)
    val result = watch(keyBytes)(expectedValue == _) { txn ⇒
      txn.del(keyBytes)
    }
    result.isDefined
  }

  def replace(key: K, newValue: V): Option[V] = {
    val keyBytes = keySer.forth(key)
    val result = watch(keyBytes)(old ⇒ true) { txn ⇒
      txn.set(keyBytes, valueSer.forth(newValue))
    }
    result
  }
  def replace(key: K, oldValue: V, newValue: V): Boolean = {
    val keyBytes = keySer.forth(key)
    val result = watch(keyBytes)(oldValue == _) { txn ⇒
      txn.set(keyBytes, valueSer.forth(newValue))
    }
    result.isDefined
  }

}