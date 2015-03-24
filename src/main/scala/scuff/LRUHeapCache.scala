package scuff

import java.util.concurrent.locks.{ ReadWriteLock, ReentrantReadWriteLock }
import scala.concurrent.duration._

/**
 * Fully thread-safe LRU cache implementation that relies on a
 * [[java.util.concurrent.locks.ReadWriteLock]] for concurrency
 * control. All time-to-live (ttl) variables are expected to be
 * in seconds.
 * NOTICE: Objects are stored and returned as-is. If they are mutable,
 * then manual care must be taken to ensure safe copy on both storing
 * and retrieving, or otherwise general usage must ensure that objects
 * stored and retrieved are not modified.
 *
 * @param maxCapacity The maximum number of entries allowed.
 * @param defaultTTL The default time-to-live. 0 means immortal.
 * @param staleCheckFreq Frequency in seconds of background thread checking for stale cache entries. Defaults to 10 seconds.
 * @param lock Optional. Alternative read/write lock.
 */
final class LRUHeapCache[K, V](maxCapacity: Int, val defaultTTL: FiniteDuration = Duration.Zero, staleCheckFreq: FiniteDuration = 10.seconds, lock: ReadWriteLock = new ReentrantReadWriteLock)
    extends Cache[K, V] with Expiry[K, V] {

  @inline implicit private def Millis = scala.concurrent.duration.MILLISECONDS
  @inline private def clock = Clock.System

  override def toString = readLock(map.toString)

  private[this] val map = new LRUMap

  private var isShutdown = false
  private def checkShutdown() = if (isShutdown) throw new IllegalStateException("Cache has been disabled.")

  private def writeLock[T](doIt: => T): T = {
    lock.writeLock.lock()
    try {
      checkShutdown()
      doIt
    } finally {
      lock.writeLock.unlock()
    }
  }

  private def readLock[T](doIt: => T): T = {
    lock.readLock.lock()
    try {
      checkShutdown()
      doIt
    } finally {
      lock.readLock.unlock()
    }
  }

  private def storeInsideLock(key: K, value: V, ttlSeconds: Int) = {
    if (scavenger.isEmpty && ttlSeconds > 0) {
      val thread = new Scavenger
      scavenger = Some(thread)
      thread.start()
    }
    map.put(key, new CacheEntry(value, ttlSeconds))
    value
  }

  def store(key: K, value: V, ttl: FiniteDuration) = writeLock(storeInsideLock(key, value, ttl.toSeconds.toInt))
  def evict(key: K): Boolean = writeLock(returnValue(map.remove(key)).nonEmpty)
  def lookupAndEvict(key: K): Option[V] = writeLock(returnValue(map.remove(key)))
  def lookup(key: K): Option[V] = readLock(returnValue(map.get(key)))
  def contains(key: K): Boolean = readLock(map.containsKey(key))

  def refresh(key: K, ttl: FiniteDuration): Boolean = lookupAndRefresh(key, ttl).isDefined
  def lookupAndRefresh(key: K, ttl: FiniteDuration): Option[V] = readLock {
    map.get(key) match {
      case null => None
      case entry if !entry.isStale() =>
        entry.refresh(ttl.toSeconds.toInt)
        entry.value
      case _ => None
    }
  }

  def shutdown() = writeLock {
    scavenger.foreach(_.interrupt)
    isShutdown = true
    map.clear()
  }

  def lookupOrStore(key: K, ttl: FiniteDuration)(construct: => V): V = {
    lookup(key) match {
      case Some(value) => value
      case None => writeLock {
        returnValue(map.get(key)) match {
          case Some(value) => value
          case None => storeInsideLock(key, construct, ttl.toSeconds.toInt)
        }
      }
    }
  }

  private def returnValue(entry: CacheEntry) = if (entry == null || entry.isStale()) None else entry.value

  private class LRUMap extends java.util.LinkedHashMap[K, CacheEntry](32, 0.75f, true) {
    override def removeEldestEntry(eldest: java.util.Map.Entry[K, CacheEntry]) = size > maxCapacity
  }

  private class CacheEntry(val value: Some[V], ttl: Int) {
    def this(value: V, ttl: Int) = this(Some(value), ttl)

    refresh(ttl)

    @volatile var expiryMillis: Long = _
    def isStale(now: Long = clock.now) = expiryMillis < now
    def refresh(ttl: Int) = expiryMillis = if (ttl > 0) clock.now + ttl * 1000 else Long.MaxValue

  }

  private var scavenger: Option[Thread] = None

  private class Scavenger extends Thread("%s expiry scavenger".format(LRUHeapCache.this.getClass.getSimpleName)) {
    import collection.JavaConverters._

    setDaemon(true)

    val sleepTimeMs = staleCheckFreq.toMillis

    def sleep() = try {
      Thread.sleep(sleepTimeMs)
    } catch {
      case _: InterruptedException =>
        writeLock(map.clear())
        this.interrupt()
    }

    override def run {
      while (!isInterrupted) {
        val staleKeys = readLock {
          val now = clock.now
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
