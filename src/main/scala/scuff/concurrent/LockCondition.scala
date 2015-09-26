package scuff.concurrent

import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.locks.Condition

/**
  * Integrated Lock/Condition object, which encapsulates the
  * common lock/condition idiom.
  *
  * Canonical example:
  * {{{
  * class RingBuffer[X] {
  *   val items = new Array[X](100)
  *   var putPtr, takePtr, count = 0
  *   val (notFull, notEmpty) = LockCondition(count < items.length, count > 0)
  *
  *   def put(x: X): Unit = {
  *     notFull {
  *       items(putPtr) = x
  *       putPtr += 1
  *       if (putPtr == items.length) putPtr = 0
  *       count += 1
  *       notEmpty.signal()
  *     }
  *   }
  *
  *   def take(): X = {
  *     notEmpty {
  *       val x = items(takePtr)
  *       takePtr += 1
  *       if (takePtr == items.length) takePtr = 0
  *       count -= 1
  *       notFull.signal()
  *       x
  *     }
  *   }
  * }
  * }}}
  */
final class LockCondition(condition: => Boolean, lock: Lock, cond: Condition) {
  def this(condition: => Boolean, lock: Lock) = this(condition, lock, lock.newCondition)

  def apply[@specialized T](thunk: => T): T = {
    lock.lockInterruptibly()
    try {
      while (!condition) cond.await()
      thunk
    } finally {
      lock.unlock()
    }
  }

  def signal() = cond.signal()
  def signalAll() = cond.signalAll()

}

object LockCondition {
  def apply(condition: => Boolean): LockCondition = new LockCondition(condition, new ReentrantLock)
  def apply(condition1: => Boolean, condition2: => Boolean): (LockCondition, LockCondition) = {
    val lock = new ReentrantLock
    new LockCondition(condition1, lock) -> new LockCondition(condition2, lock)
  }
  def apply(condition1: => Boolean, condition2: => Boolean, condition3: => Boolean): (LockCondition, LockCondition, LockCondition) = {
    val lock = new ReentrantLock
    (new LockCondition(condition1, lock), new LockCondition(condition2, lock), new LockCondition(condition3, lock))
  }
  def apply(condition1: => Boolean, condition2: => Boolean, condition3: => Boolean, condition4: => Boolean): (LockCondition, LockCondition, LockCondition, LockCondition) = {
    val lock = new ReentrantLock
    (new LockCondition(condition1, lock), new LockCondition(condition2, lock), new LockCondition(condition3, lock), new LockCondition(condition4, lock))
  }
}
