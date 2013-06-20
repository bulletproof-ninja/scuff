package scuff

/**
 * Spin lock. Useful for code that wants to avoid
 * context switching on contention.
 */
final class SpinLock {
  private[this] val lock = new java.util.concurrent.atomic.AtomicBoolean(false)

  def whenLocked[T](code: ⇒ T): T = {
    while (!lock.compareAndSet(false, true)) {}
    try {
      code
    } finally {
      lock.set(false)
    }
  }
}