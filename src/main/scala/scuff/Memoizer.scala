package scuff

import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReentrantLock
import scuff.concurrent.LockFreeConcurrentMap

/**
  * Class that guarantees exactly one instance created per
  * argument (based on `equals` equality).
  * NOTICE: [[scala.collection.concurrent.Map#getOrElseUpdate]]
  * can provide similar behavior, but cannot guarantee a single
  * instance per key, due to inherent race conditions.
  */
class Memoizer[A, R](make: A => R) {

  private val locks = new collection.concurrent.TrieMap[A, Lock]
  private def synch(arg: A)(thunk: => R): R = {
    val lock = locks.getOrElseUpdate(arg, new ReentrantLock)
    lock.lock()
    try {
      thunk
    } finally {
      lock.unlock
      locks.remove(arg, lock)
    }
  }
  private[this] val map = new LockFreeConcurrentMap[A, R]

  def apply(arg: A): R = {
    map.get(arg) match {
      case Some(res) => res
      case None =>
        synch(arg) {
          map.getOrElseUpdate(arg, make(arg))
        }
    }
  }

}
