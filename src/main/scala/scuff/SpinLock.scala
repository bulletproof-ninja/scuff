package scuff

/**
 * Reentrant spin lock. Useful for code that wants to avoid
 * context switching on rare or no contention.
 * NOTICE: This should NOT be used on highly (or even moderately)
 * contended resources.
 */
final class SpinLock {
  private[this] val lock = new java.util.concurrent.atomic.AtomicReference[Thread]

  @annotation.tailrec
  def whenLocked[T](code: => T): T = {
    if (lock.compareAndSet(null, Thread.currentThread)) {
      try {
        code
      } finally {
        lock.set(null)
      }
    } else if (lock.get() == Thread.currentThread) {
      code
    } else {
      whenLocked(code)
    }
  }

}
