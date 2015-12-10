package scuff

import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReentrantLock
import scuff.concurrent.LockFreeConcurrentMap

/**
  * Class that guarantees exactly one instance created per
  * argument (based on `equals` equality).
  * NOTICE: [[scala.collection.concurrent.Map#getOrElse]] and
  * [[scala.collection.concurrent.Map#putIfAbsent]] can provide
  * similar behavior, but cannot guarantee a single instance per
  * key, due to inherent race conditions.
  */
class Memoizer[A, R](make: A => R) {

  private val locks = new collection.concurrent.TrieMap[A, Lock]
  private def synchronized(arg: A)(thunk: => R): R = {
    val lock = locks.get(arg) getOrElse {
      val lock = new ReentrantLock
      locks.putIfAbsent(arg, lock) getOrElse lock
    }
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
        synchronized(arg) {
          map.getOrElseUpdate(arg, make(arg))
        }
    }
  }

}
