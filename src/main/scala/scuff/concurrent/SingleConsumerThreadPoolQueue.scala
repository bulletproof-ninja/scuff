package scuff.concurrent

import java.util.concurrent.BlockingQueue
import java.util.concurrent.TimeUnit

private[concurrent] class SingleConsumerThreadPoolQueue(
    underlying: BlockingQueue[Runnable])
  extends BlockingQueue[Runnable] {

  private[this] var consumerThread: Thread = _

  @inline
  private def supply(r: Runnable, supplier: Runnable => Boolean): Boolean = {
    if (Thread.currentThread eq consumerThread) {
      underlying.poll() match {
        case null =>
          r.run()
          true
        case older =>
          val result = supplier(r)
          older.run()
          result
      }
    } else supplier(r)
  }

  private[this] val Add: (Runnable => Boolean) = underlying add _
  def add(r: Runnable): Boolean = supply(r, Add)
  def contains(obj: Object): Boolean = underlying contains obj
  def drainTo(coll: java.util.Collection[_ >: Runnable]): Int = {
    consumerThread = Thread.currentThread
    underlying drainTo coll
  }
  def drainTo(coll: java.util.Collection[_ >: Runnable], maxElements: Int): Int = {
    consumerThread = Thread.currentThread
    underlying.drainTo(coll, maxElements)
  }
  private[this] val Offer: (Runnable => Boolean) = underlying offer _
  def offer(r: Runnable): Boolean = supply(r, Offer)
  private[this] def OfferWTO(timeout: Long, unit: TimeUnit): (Runnable => Boolean) = underlying.offer(_, timeout, unit)
  def offer(r: Runnable, timeout: Long, unit: TimeUnit): Boolean = supply(r, OfferWTO(timeout, unit))
  def poll(timeout: Long, unit: TimeUnit): Runnable = {
    consumerThread = Thread.currentThread
    underlying.poll(timeout, unit)
  }
  private[this] val Put: (Runnable => Boolean) = r => { underlying.put(r); true }
  def put(r: Runnable): Unit = supply(r, Put)
  def remainingCapacity(): Int = underlying.remainingCapacity()
  def remove(obj: Object): Boolean = {
    consumerThread = Thread.currentThread
    underlying remove obj
  }
  def take(): Runnable = {
    consumerThread = Thread.currentThread
    underlying.take()
  }

  def element(): Runnable = {
    consumerThread = Thread.currentThread
    underlying.element()
  }
  def peek(): Runnable = {
    consumerThread = Thread.currentThread
    underlying.peek()
  }
  def poll(): Runnable = {
    consumerThread = Thread.currentThread
    underlying.poll()
  }
  def remove(): Runnable = {
    consumerThread = Thread.currentThread
    underlying.remove()
  }

  def addAll(coll: java.util.Collection[_ <: Runnable]) = {
    import collection.JavaConverters._
    if (Thread.currentThread eq consumerThread) {
      coll.iterator.asScala.foldLeft(false) {
        case (changed, r) => changed | add(r)
      }
    } else underlying addAll coll
  }
  def clear() = underlying.clear()
  def containsAll(coll: java.util.Collection[_]) = underlying containsAll coll
  override def equals(obj: Any) = underlying equals obj
  override def hashCode() = underlying.hashCode()
  def isEmpty() = underlying.isEmpty()
  def iterator() = underlying.iterator()
  def removeAll(coll: java.util.Collection[_]) = {
    consumerThread = Thread.currentThread
    underlying removeAll coll
  }
  def retainAll(coll: java.util.Collection[_]) = underlying retainAll coll
  def size() = underlying.size
  def toArray() = underlying.toArray
  def toArray[T](arr: Array[T with Object]) = underlying toArray[T] arr

}
