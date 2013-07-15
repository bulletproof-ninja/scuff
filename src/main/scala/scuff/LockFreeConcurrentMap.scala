package scuff

final class LockFreeConcurrentMap[A, B](initialMap: Map[A, B] = Map[A, B]()) extends collection.concurrent.Map[A, B] {

  require(initialMap != null, "Initial map cannot be null")

  private[this] val mapRef = new java.util.concurrent.atomic.AtomicReference(initialMap)

  @annotation.tailrec
  def putIfAbsent(k: A, v: B): Option[B] = {
    val map = mapRef.get
    map.get(k) match {
      case existing @ Some(_) ⇒ existing
      case None ⇒
        val updated = map + (k -> v)
        if (mapRef.compareAndSet(map, updated)) {
          None
        } else {
          putIfAbsent(k, v)
        }
    }
  }

  @annotation.tailrec
  def remove(k: A, expected: B): Boolean = {
    val map = mapRef.get
    map.get(k) match {
      case Some(value) if value == expected ⇒
        val updated = map - k
        if (mapRef.compareAndSet(map, updated)) {
          true
        } else {
          remove(k, expected)
        }
      case _ ⇒ false
    }
  }

  @annotation.tailrec
  def replace(k: A, expected: B, newvalue: B): Boolean = {
    val map = mapRef.get
    map.get(k) match {
      case None ⇒ false
      case Some(value) if value == expected ⇒
        val updated = map + (k -> newvalue)
        if (mapRef.compareAndSet(map, updated)) {
          true
        } else {
          replace(k, expected, newvalue)
        }
      case _ ⇒ false
    }
  }

  @annotation.tailrec
  def replace(k: A, v: B): Option[B] = {
    val map = mapRef.get
    map.get(k) match {
      case None ⇒ None
      case replaced @ Some(_) ⇒
        val updated = map + (k -> v)
        if (mapRef.compareAndSet(map, updated)) {
          replaced
        } else {
          replace(k, v)
        }
    }
  }

  @annotation.tailrec
  def -=(k: A): this.type = {
    val map = mapRef.get
    val updated = map - k
    if (mapRef.compareAndSet(map, updated)) {
      this
    } else {
      this.-=(k)
    }
  }

  @annotation.tailrec
  def +=(kv: (A, B)): this.type = {
    val map = mapRef.get
    val updated = map + kv
    if (mapRef.compareAndSet(map, updated)) {
      this
    } else {
      this.+=(kv)
    }
  }

  def iterator() = mapRef.get.iterator
  def get(k: A) = mapRef.get.get(k)

  override def size = mapRef.get.size

  @annotation.tailrec
  def removeAll(keys: A*): this.type = {
    if (keys.isEmpty) {
      this
    } else {
      val map = mapRef.get
      var updated = map
      keys.foreach(updated -= _)
      if (mapRef.compareAndSet(map, updated)) {
        this
      } else {
        removeAll(keys: _*)
      }
    }
  }

  @annotation.tailrec
  def putAll(copyMap: collection.Map[A, B]): this.type = {
    if (copyMap.isEmpty) {
      this
    } else {
      val map = mapRef.get
      var updated = map
      copyMap.foreach(updated += _)
      if (mapRef.compareAndSet(map, updated)) {
        this
      } else {
        putAll(copyMap)
      }
    }
  }

  override def clear {
    mapRef.set(mapRef.get.empty)
  }

  def snapshot() = mapRef.get

  override def toString() = "LockFreeConcurrent" concat super.toString()

}
