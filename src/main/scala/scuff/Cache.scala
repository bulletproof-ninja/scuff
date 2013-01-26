package scuff

/**
  * Generic cache interface.
  */
trait Cache[K, V] {
  /**
    * Default time-to-live, in seconds.
    * Can be overridden per entry when storing.
    * 0 means no expiry.
    */
  def defaultTTL: Int
  /**
    * Store or replace value.
    * @param key Cache key
    * @param value Value to cache
    * @param ttl Time-to-live. Optional. Defaults to [[defaultTTL]].
    */
  def store(key: K, value: V, ttlSeconds: Int = defaultTTL)

  /**
    * Evict cache entry.
    */
  def evict(key: K): Option[V]

  /**
    * Lookup cache entry.
    */
  def lookup(key: K): Option[V]

  /**
    * Lookup cache entry, or if not found, construct, store, and return entry.
    * @param key
    * @param constructor The value constructor, if key not found
    * @param ttlSeconds time-to-live if stored. Defaults to [[defaultTTL]]
    */
  def lookupOrStore(key: K, ttlSeconds: Int = defaultTTL)(constructor: ⇒ V): V

  /**
    * Disable cache. This will permanently disable the cache and purge all entries.
    * Further attempts to use the cache will result in a [[IllegalStateException]].
    */
  def disable()
}