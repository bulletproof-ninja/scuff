package scuff.concurrent

import scala.collection.immutable.{ Set, HashSet }
import annotation.tailrec
import scala.jdk.CollectionConverters._

/**
  * Concurrent multi-map implementation.
  */
class MultiMap[K, V] extends Iterable[(K, Set[V])] {

  private[this] val map = new java.util.concurrent.ConcurrentHashMap[K, HashSet[V]]

  private def getValues(key: K): HashSet[V] =
    map.get(key) match {
      case null => HashSet.empty
      case set => set
    }

  def keys: Iterator[K] = map.keySet.iterator.asScala
  def keySet: collection.Set[K] = java.util.Collections.unmodifiableSet(map.keySet).asScala

  /**
    * Abstraction over key values.
    */
  final class Values private[MultiMap] (key: K) extends Iterable[V] {
    def iterator = getValues(key).iterator
    def contains(value: V) = getValues(key).contains(value)
    def +=(value: V): Boolean = add(key, value)
    def -=(value: V): Boolean = remove(key, value)
    def ++=(values: Iterable[V]): Unit = add(key, values)
    def --=(values: Iterable[V]): Unit = remove(key, values)
  }

  def apply(key: K): Values = new Values(key)
  /**
   * Get values for key. Will always return a `Set`.
   * @param key The key
   * @return Set of values. Will be empty for unknown key.
   */
  def get(key: K): Set[V] = getValues(key)

  def containsKey(key: K): Boolean = map.containsKey(key)

  /**
    *  Update (replace) single value for key.
    *  @param key The key
    *  @param value The new single value
    *  @return  The replaced values, if any
    */
  def update(key: K, value: V): Set[V] = update(key, List(value))
  /**
    *  Update (replace) values for key.
    *  @param key The key
    *  @param values The new values
    *  @return  The replaced values, if any
    */
  def update(key: K, values: Iterable[V]): Set[V] = {
    map.put(key, HashSet.empty ++ values) match {
      case null => Set.empty
      case set => set
    }
  }

  /**
    *  Add value for key.
    *  @return `true` if added, `false` if already exists
    */
  def add(key: K, value: V): Boolean = {
      @tailrec def tryAdd(): Boolean = {
        map.putIfAbsent(key, HashSet(value)) match {
          case null => true
          case existing if existing(value) => false
          case existing =>
            if (map.replace(key, existing, existing + value)) true
            else tryAdd()
        }
      }
    tryAdd()
  }

  /** Add values for key. */
  def add(key: K, values: Iterable[V]): Unit = {
      @tailrec def tryAdd(): Unit = {
        val existing = getValues(key)
        val updated = existing ++ values
        if (existing.isEmpty) {
          map.putIfAbsent(key, updated) match {
            case null => // Ok
            case existing =>
              if (!map.replace(key, existing, updated)) tryAdd()
          }
        } else {
          if (!map.replace(key, existing, updated)) tryAdd()
        }
      }
    tryAdd()
  }

  /**
    *  Remove value for key.
    *  @return `true` if removed, `false` if non-existent
    */
  def remove(key: K, value: V): Boolean = {
      @tailrec def tryRemove(): Boolean = {
        val existing = getValues(key)
        if (existing.isEmpty) false
        else {
          val newSet = existing - value
          if (newSet.isEmpty) {
            if (map.remove(key, existing)) true
            else tryRemove()
          } else { // Not empty
            if (map.replace(key, existing, newSet)) existing(value)
            else tryRemove()
          }
        }
      }
    tryRemove()
  }

  /** Remove values for key. */
  def remove(key: K, values: Iterable[V]): Unit = {
      @tailrec def tryRemove(): Unit = {
        val existing = getValues(key)
        if (existing.nonEmpty) {
          val updated = values.foldLeft(existing) {
            case (set, value) => set - value
          }
          if (updated.isEmpty) {
            if (!map.remove(key, existing)) tryRemove()
          } else {
            if (!map.replace(key, existing, updated)) tryRemove()
          }
        }
      }
    tryRemove()
  }

  /** Remove all values for key. */
  def removeAll(key: K): Unit = map.remove(key)

  /** Clear entire map. */
  def clear(): Unit = map.clear()

  def iterator: Iterator[(K, Set[V])] = new Iterator[(K, Set[V])] {
    private[this] val entries = MultiMap.this.map.entrySet.iterator
    def hasNext: Boolean = entries.hasNext
    def next: (K, Set[V]) = {
      val entry = entries.next
      (entry.getKey, entry.getValue)
    }
  }
}
