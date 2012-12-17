package scuff

import java.util.concurrent.locks.{ ReadWriteLock, ReentrantReadWriteLock }

/**
 * Fully thread-safe LRU cache implementation that relies on a 
 * [[java.util.concurrent.locks.ReadWriteLock]] for concurrency
 * control. All time-to-live (ttl) variables are expected to be 
 * in seconds.
 *
 * @param maxCapacity The maximum number of entries allowed.
 * @param defaultTTL The default time-to-live. 0 means immortal.
 * @param staleCheckFreq Frequency in seconds of background thread checking for stale cache entries. Defaults to 10 seconds.
 * @param lock Optional. Alternative read/write lock.
 */
final class LRUHeapCache[K, V](maxCapacity: Int, val defaultTTL: Int, staleCheckFreq: Int = 10, lock: ReadWriteLock = new ReentrantReadWriteLock) extends Cache[K, V] {

  override def toString = map.toString
  
  private[this] val map = new LRUMap

  private var disabled = false
  private def checkDisabled = if (disabled) throw new IllegalStateException("Cache has been disabled.")

  private def writeLock[T](doIt: ⇒ T): T = {
    lock.writeLock.lock()
    try {
      checkDisabled
      doIt
    } finally {
      lock.writeLock.unlock()
    }
  }

  private def readLock[T](doIt: ⇒ T): T = {
    lock.readLock.lock()
    try {
      checkDisabled
      doIt
    } finally {
      lock.readLock.unlock()
    }
  }

  private def storeInsideLock(key: K, value: V, ttl: Int) = {
    if (scavenger.isEmpty && ttl > 0) {
      val thread = new Scavenger
      scavenger = Some(thread)
      thread.start()
    }
    map.put(key, new CacheEntry(value, ttl))
    value
  }

  def store(key: K, value: V, ttl: Int) = writeLock(storeInsideLock(key, value, ttl))
  def evict(key: K): Option[V] = writeLock(returnValue(map.remove(key)))
  def lookup(key: K): Option[V] = readLock(returnValue(map.get(key)))

  def disable() = writeLock {
    scavenger.foreach(_.interrupt)
    disabled = true
    map.clear()
  }

  def lookupOrStore(key: K, ttl: Int)(construct: ⇒ V): V = {
    lookup(key) match {
      case Some(value) ⇒ value
      case None ⇒ writeLock {
        returnValue(map.get(key)) match {
          case Some(value) ⇒ value
          case None ⇒ storeInsideLock(key, construct, ttl)
        }
      }
    }
  }

  private def returnValue(entry: CacheEntry) = if (entry == null || entry.isStale()) None else entry.value

  private class LRUMap extends java.util.LinkedHashMap[K, CacheEntry](32, 0.75f, true) {
    override def removeEldestEntry(eldest: java.util.Map.Entry[K, CacheEntry]) = size > maxCapacity
  }

  private class CacheEntry(rawValue: V, ttl: Int) {
    val value = Some(rawValue)
    val expiryMillis = if (ttl > 0) System.currentTimeMillis + ttl*1000 else Long.MaxValue
    def isStale(now: Long = System.currentTimeMillis) = expiryMillis < now
  }

  private var scavenger: Option[Thread] = None

  private class Scavenger extends Thread("LRUHeapCache expiry scavenger") {
    import collection.JavaConverters._

    setDaemon(true)

    val sleepTime = staleCheckFreq * 1000

    def sleep() = try {
      Thread.sleep(sleepTime)
    } catch {
      case _: InterruptedException ⇒
        writeLock(map.clear())
        this.interrupt()
    }

    override def run {
      while (!isInterrupted) {
        val staleKeys = readLock {
          val now = System.currentTimeMillis
          map.entrySet.asScala.withFilter(_.getValue.isStale(now)).map(_.getKey)
        }
        if (!staleKeys.isEmpty) {
          writeLock(staleKeys.foreach(map.remove))
        }
        sleep()
      }
    }
  }

}