package scuff

import java.util.ArrayDeque

/**
  * Simple bounded circular queue that
  * replaces the oldest entry with the newest
  * when capacity is reached.
  * NOTE: This implementation is NOT thread-safe
  * if used concurrently.
  */
final class CircularBuffer[T](capacity: Int) {
  private[this] val deque = new ArrayDeque[T](capacity)

  /** Append to tail of queue. */
  def append(t: T) {
    if (deque.size == capacity) {
      deque.pollFirst()
    }
    deque.addLast(t)
  }

  def contains(elem: T): Boolean = deque.contains(elem)

  /** Remove head of queue. */
  def next(): Option[T] = Option(deque.pollFirst)
  /** Remove head of queue, returning `null` if empty. */
  def nextOrNull(): T = deque.pollFirst()

  def isEmpty = deque.isEmpty
  def size = deque.size
}
