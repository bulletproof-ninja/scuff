package scuff.concurrent

import org.junit._, Assert._
import scala.concurrent.duration._
import scala.util.Try
import scuff.Numbers

class TestCircuitBreaker {

  def fibonnaciBackoff(maxBackoff: FiniteDuration) =
    Numbers.fibonacci.view.dropWhile(_ < 2).map(_.seconds).takeWhile(_ <= maxBackoff)

  private var errors: List[Throwable] = null

  @Before
  def setup() = errors = Nil

  def reportError(th: Throwable): Unit = errors = th :: errors

  @Test(expected = classOf[IllegalArgumentException])
  def `no backoff schedule`(): Unit = {
    val cb = new CircuitBreaker(3, reportError, Nil)
    fail(s"Should have failed: $cb")
  }

  @Test(expected = classOf[IllegalArgumentException])
  def `zero failure count threshold`(): Unit = {
    val cb = new CircuitBreaker(0, reportError, List(2.minutes))
    fail(s"Should have failed: $cb")
  }

  @Test
  def `finite backoff schedule`(): Unit = {
    val error = Try(sys.error("oh no")).failed.get
    val threshold = 3
    val backoffSchedule = fibonnaciBackoff(2.minutes)
    val scheduleLength = backoffSchedule.size

    val cb = new CircuitBreaker(threshold, reportError, backoffSchedule)
    assertEquals(0, cb.failureCount)
    assertFalse(cb.isTripped)
    assertEquals(Duration.Zero, cb.timeout)

    val backoffIterator = backoffSchedule.iterator
    var currTimeout = backoffIterator.next

    (1 to scheduleLength + 5) foreach { n =>
      cb reportFailure error
      assertEquals(n, cb.failureCount)
      if (cb.failureCount < threshold) {
        assertFalse(cb.isTripped)
        assertEquals(Duration.Zero, cb.timeout)
      } else {
        assertTrue(cb.isTripped)
        assertEquals(currTimeout, cb.timeout)
        if (backoffIterator.hasNext) currTimeout = backoffIterator.next
      }
    }
    assertEquals(threshold, errors.size)
    errors.foreach { err =>
      assertSame(error, err)
    }
    cb.reset()
    assertEquals(0, cb.failureCount)
    assertFalse(cb.isTripped)
    assertEquals(Duration.Zero, cb.timeout)

  }

}
