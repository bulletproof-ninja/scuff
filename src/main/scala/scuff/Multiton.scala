package scuff

/**
 * Class that guarantees exactly one instance per key being created.
 * This is known as a
 *  <a href="http://en.wikipedia.org/wiki/Multiton">Multiton</a>.
 * NOTICE: [[ConcurrentMap#getOrElse]] and [[ConcurrentMap#putIfAbsent]] provides 
 * similar behavior, but does not guarantee a single instance per key, 
 * due to race conditions.
 */
class Multiton[K, V](factory: K ⇒ V) {

  @volatile
  private var map: Map[K, V] = Map.empty
  private def getOrNull(key: K) = map.getOrElse(key, null.asInstanceOf[V])

  /**
   * Return value associated with key, and if not already constructed,
   * call factory and store.
   * @param key The key to use for lookup
   * @return The singleton value associated with the key
   */
  def apply(key: K): V = {
    var value = getOrNull(key)
    if (value == null) {
      this.synchronized {
        value = getOrNull(key)
        // DCL for the WIN!
        if (value == null) {
          value = factory(key)
          map += (key -> value)
        }
      }
    }
    value
  }
}
