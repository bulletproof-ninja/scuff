package scuff.concurrent

/**
 * Reentrant spin lock. Useful for code that wants to avoid
 * context switching on rare or no contention.
 * NOTICE: This should probably not be used on highly
 * (or even moderately) contended resources, nor if the code
 * under lock is not trivial (e.g. blocking operations),
 * unless you want to increase your electricity bill.
 */
final class SpinLock {
  private[this] val lock = new java.util.concurrent.atomic.AtomicReference[Thread]

  @annotation.tailrec
  def apply[T](thunk: => T): T = {
    if (lock.compareAndSet(null, Thread.currentThread)) {
      try thunk finally {
        lock.set(null)
      }
    } else if (lock.get eq Thread.currentThread) {
      thunk
    } else {
      apply(thunk)
    }
  }

  override def toString() = s"SpinLock(owner = ${lock.get})"
}
