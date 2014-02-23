package scuff

import java.util.concurrent.locks._
import java.nio._

/**
  * This cache allows you to store arbitrary sized objects off 
  * the JVM heap, thereby not affecting GC times.
  */
final class LRUDirectMemoryCache[K, V](maxCapacity: Int, val defaultTTL: Int, ser: Serializer[V], staleCheckFreq: Int = 10, lock: ReadWriteLock = new ReentrantReadWriteLock)
    extends Cache[K, V] {

  private[this] val impl = new LRUHeapCache[K, ByteBuffer](maxCapacity, defaultTTL, staleCheckFreq, lock)

  private def toValue(buffer: ByteBuffer): V = {
    val bytes = new Array[Byte](buffer.capacity)
    buffer.synchronized {
      buffer.position(0)
      buffer.get(bytes)
    }
    ser.decode(bytes)
  }

  private def toBuffer(value: V): ByteBuffer = {
    val bytes = ser.encode(value)
    val buffer = ByteBuffer.allocateDirect(bytes.length)
    buffer.put(bytes).asReadOnlyBuffer()
  }

  def store(key: K, value: V, ttlSeconds: Int) = impl.store(key, toBuffer(value), ttlSeconds)
  def evict(key: K): Option[V] = impl.evict(key).map(toValue)
  def lookup(key: K): Option[V] = impl.lookup(key).map(toValue)
  def lookupOrStore(key: K, ttlSeconds: Int)(constructor: ⇒ V): V = toValue(impl.lookupOrStore(key, ttlSeconds)(toBuffer(constructor)))
  def disable() = impl.disable()
  def lookupAndRefresh(key: K, ttl: Int): Option[V] = impl.lookupAndRefresh(key, ttl).map(toValue)
  def refresh(key: K, ttl: Int) = impl.refresh(key, ttl)
}
