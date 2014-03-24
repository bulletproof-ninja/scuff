package scuff

/**
 * Generic cache interface.
 */
trait Cache[K, V] {
  /**
   * Store or replace value.
   * @param key Cache key
   * @param value Value to cache
   */
  def store(key: K, value: V)

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
   */
  def lookupOrStore(key: K)(constructor: ⇒ V): V

  /**
   * Shutdown cache. This will permanently disable the cache and purge all entries.
   * Further attempts to use the cache will result in a [[IllegalStateException]].
   */
  def shutdown()
}

/**
 * Time sensitive cache.
 */
trait Expiry[K, V] { self: Cache[K, V] =>
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
  def store(key: K, value: V, ttlSeconds: Int)
  final def store(key: K, value: V): Unit = store(key, value, defaultTTL)

  /**
   * Refresh TTL. If no TTL is supplied,
   * the default TTL will be used, NOT the
   * TTL supplied when stored (if any).
   * Return `true` if successful.
   */
  def refresh(key: K, ttl: Int = defaultTTL): Boolean
  /**
   * Lookup cache entry and refresh TTL.
   * If no TTL is supplied,
   * the default TTL will be used, NOT the
   * TTL supplied when stored (if any).
   */
  def lookupAndRefresh(key: K, ttl: Int = defaultTTL): Option[V]

  /**
   * Lookup cache entry, or if not found, construct, store, and return entry.
   * @param key
   * @param constructor The value constructor, if key not found
   * @param ttlSeconds time-to-live if stored. Defaults to [[defaultTTL]]
   */
  def lookupOrStore(key: K, ttlSeconds: Int)(constructor: ⇒ V): V
  final def lookupOrStore(key: K)(constructor: ⇒ V): V = lookupOrStore(key, defaultTTL)(constructor)

}
