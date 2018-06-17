package scuff.concurrent

import scala.collection.immutable.Map

/**
  * Lock-free concurrent Map.
  * Wrapper-class that turns any immutable Map into a
  * lock-free concurrent map.
  */
final class LockFreeConcurrentMap[A, B](initialMap: Map[A, B] = Map[A, B]())
  extends collection.concurrent.Map[A, B] {

  require(initialMap != null, "Initial map cannot be null")

  private[this] val EmptyMap = initialMap.empty

  private[this] val mapRef = new java.util.concurrent.atomic.AtomicReference(initialMap)

  @annotation.tailrec
  final def putIfAbsent(k: A, v: B): Option[B] = {
    val map = mapRef.get
    map.get(k) match {
      case existing: Some[_] => existing
      case _ =>
        val updated = map + (k -> v)
        if (mapRef.compareAndSet(map, updated)) {
          None
        } else {
          putIfAbsent(k, v)
        }
    }
  }

  @annotation.tailrec
  final def remove(k: A, expected: B): Boolean = {
    val map = mapRef.get
    map.get(k) match {
      case Some(value) if value == expected =>
        val updated = map - k
        if (mapRef.compareAndSet(map, updated)) {
          true
        } else {
          remove(k, expected)
        }
      case _ => false
    }
  }

  @annotation.tailrec
  final def replace(k: A, expected: B, newvalue: B): Boolean = {
    val map = mapRef.get
    map.get(k) match {
      case Some(value) if value == expected =>
        val updated = map + (k -> newvalue)
        if (mapRef.compareAndSet(map, updated)) {
          true
        } else {
          replace(k, expected, newvalue)
        }
      case _ => false
    }
  }

  @annotation.tailrec
  final def replace(k: A, v: B): Option[B] = {
    val map = mapRef.get
    map.get(k) match {
      case replaced: Some[_] =>
        val updated = map + (k -> v)
        if (mapRef.compareAndSet(map, updated)) {
          replaced
        } else {
          replace(k, v)
        }
      case _ => None
    }
  }

  def subtractOne(k: A): this.type = {
      @annotation.tailrec
      def tryRemove(): Unit = {
        val map = mapRef.get
        val updated = map - k
        if (!mapRef.compareAndSet(map, updated)) {
          tryRemove()
        }
      }
    tryRemove()
    this
  }

  def addOne(kv: (A, B)): this.type = {
      @annotation.tailrec
      def tryAdd(): Unit = {
        val map = mapRef.get
        val updated = map + kv
        if (!mapRef.compareAndSet(map, updated)) {
          tryAdd()
        }
      }
    tryAdd()
    this
  }

  def iterator = mapRef.get.iterator
  def get(k: A): Option[B] = mapRef.get.get(k)

  override def size = mapRef.get.size

  def removeAll(keys: A*): this.type = {
      @annotation.tailrec
      def tryRemoveAll(): Unit = {
        val map = mapRef.get
        val updated = map -- keys
        if (!mapRef.compareAndSet(map, updated)) {
          tryRemoveAll()
        }
      }
    if (keys.nonEmpty) tryRemoveAll()
    this
  }

  def putAll(copyMap: collection.Map[A, B]): this.type = {
      @annotation.tailrec
      def tryPutAll(): Unit = {
        val map = mapRef.get
        val updated = map ++ copyMap
        if (!mapRef.compareAndSet(map, updated)) {
          tryPutAll()
        }
      }
    if (copyMap.nonEmpty) tryPutAll()
    this
  }

  override def clear(): Unit = {
    mapRef.set(EmptyMap)
  }

  override def contains(key: A) = mapRef.get.contains(key)
  override def isEmpty = mapRef.get.isEmpty

  def snapshot[M <: Map[A, B]]: M = mapRef.get.asInstanceOf[M]

  def drain[M <: Map[A, B]](): M = mapRef.getAndSet(EmptyMap).asInstanceOf[M]

  @annotation.tailrec
  final override def remove(key: A): Option[B] = {
    val map = mapRef.get
    map.get(key) match {
      case value: Some[_] =>
        val updated = map - key
        if (mapRef.compareAndSet(map, updated)) {
          value
        } else {
          remove(key)
        }
      case _ => None
    }
  }

  override def getOrElseUpdate(key: A, makeValue: => B): B = {
    get(key) match {
      case Some(value) => value
      case _ =>
        val newValue = makeValue
        putIfAbsent(key, newValue) getOrElse newValue
    }
  }

}
