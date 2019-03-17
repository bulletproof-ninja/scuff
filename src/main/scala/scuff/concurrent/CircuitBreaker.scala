package scuff.concurrent

import scala.concurrent.duration._

object CircuitBreaker {
  private final case class FailureState private (
      count: Int, nextTimeout: FiniteDuration, timeoutSchedule: Iterator[FiniteDuration]) {
    def this(timeoutSchedule: Iterator[FiniteDuration]) = this(1, timeoutSchedule.next, timeoutSchedule)
  }

  def apply(failureCountThreshold: Int, failureReporter: Throwable => Unit,
      timeoutSchedule: Iterable[FiniteDuration]): CircuitBreaker =
    new CircuitBreaker(failureCountThreshold, failureReporter, timeoutSchedule)
}

class CircuitBreaker(
    failureThreshold: Int, failureReporter: Throwable => Unit,
    timeoutSchedule: Iterable[FiniteDuration]) {

  require(failureThreshold >= 1, s"Must have failure threshold >= 1, was $failureThreshold")
  require(timeoutSchedule.nonEmpty, s"Must have at least one delay in backoff schedule")

  import CircuitBreaker.FailureState

  private[this] val writeLock = new SpinLock

  @volatile private[this] var failureState: CircuitBreaker.FailureState = null

  /** Is circuit breaker active, i.e. tracking failures? */
  def isActive: Boolean = failureState != null

  /** Is circuit breaker tripped, i.e. has failure count met threshold? */
  def isTripped: Boolean = failureCount >= failureThreshold

  def failureCount: Int = {
    val state = failureState
    if (state != null) state.count else 0
  }

  def reset(): Unit = if (failureState != null) writeLock { failureState = null }

  def timeout(): FiniteDuration = {
    val state = failureState
    if (state == null || state.count < failureThreshold) Duration.Zero
    else writeLock {
      val state = failureState
      if (state == null || state.count < failureThreshold) Duration.Zero
      else {
        if (state.timeoutSchedule.hasNext) {
          failureState = state.copy(nextTimeout = state.timeoutSchedule.next)
        }
        state.nextTimeout
      }
    }
  }

  def reportFailure(cause: Throwable): Unit = writeLock {
    val newState = {
      val oldState = failureState
      if (oldState == null) new FailureState(timeoutSchedule.iterator)
      else oldState.copy(oldState.count + 1)
    }
    if (newState.count <= failureThreshold) {
      failureReporter(cause)
    }
    failureState = newState
  }

}
