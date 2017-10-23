package scuff.concurrent

import java.util.concurrent.BlockingQueue
import java.util.concurrent.TimeUnit

private[concurrent] class SingleConsumerThreadPoolQueue(
    underlying: BlockingQueue[Runnable])
  extends BlockingQueue[Runnable] {

  private[this] var consumerThread: Thread = _

  def add(r: Runnable): Boolean = {
    if (Thread.currentThread eq consumerThread) { r.run(); true }
    else underlying add r
  }
  def contains(obj: Object): Boolean = underlying contains obj
  def drainTo(coll: java.util.Collection[_ >: Runnable]): Int = {
    consumerThread = Thread.currentThread
    underlying drainTo coll
  }
  def drainTo(coll: java.util.Collection[_ >: Runnable], maxElements: Int): Int = {
    consumerThread = Thread.currentThread
    underlying.drainTo(coll, maxElements)
  }
  def offer(r: Runnable): Boolean = {
    if (Thread.currentThread eq consumerThread) { r.run(); true }
    else underlying offer r
  }
  def offer(r: Runnable, timeout: Long, unit: TimeUnit): Boolean = {
    if (Thread.currentThread eq consumerThread) { r.run(); true }
    else underlying.offer(r, timeout, unit)
  }
  def poll(timeout: Long, unit: TimeUnit): Runnable = {
    consumerThread = Thread.currentThread
    underlying.poll(timeout, unit)
  }
  def put(r: Runnable): Unit = {
    if (Thread.currentThread eq consumerThread) r.run()
    else underlying put r
  }
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
    if (Thread.currentThread eq consumerThread) { coll.iterator.asScala.foreach(_.run); true }
    else underlying addAll coll
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
