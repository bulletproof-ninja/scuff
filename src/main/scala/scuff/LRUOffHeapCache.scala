package scuff

import java.util.concurrent.locks._
import java.nio._
import scala.concurrent.duration._

/**
 * This cache allows you to store arbitrary sized objects off
 * the JVM heap, thereby not affecting GC times.
 */
final class LRUOffHeapCache[K, V](maxCapacity: Int, ser: Serializer[V], val defaultTTL: FiniteDuration = Duration.Zero, staleCheckFreq: FiniteDuration = 10.seconds, lock: ReadWriteLock = new ReentrantReadWriteLock)
  extends Cache[K, V] with Expiry[K, V] {

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

  def store(key: K, value: V, ttl: FiniteDuration) = impl.store(key, toBuffer(value), ttl)
  def evict(key: K): Boolean = impl.evict(key)
  def lookupAndEvict(key: K): Option[V] = impl.lookupAndEvict(key).map(toValue)
  def lookup(key: K): Option[V] = impl.lookup(key).map(toValue)
  def contains(key: K): Boolean = impl.contains(key)
  def lookupOrStore(key: K, ttl: FiniteDuration)(constructor: => V): V = toValue(impl.lookupOrStore(key, ttl)(toBuffer(constructor)))
  def shutdown() = impl.shutdown()
  def lookupAndRefresh(key: K, ttl: FiniteDuration): Option[V] = impl.lookupAndRefresh(key, ttl).map(toValue)
  def refresh(key: K, ttl: FiniteDuration) = impl.refresh(key, ttl)
}
