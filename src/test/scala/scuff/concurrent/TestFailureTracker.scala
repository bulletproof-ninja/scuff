package scuff.concurrent

import org.junit._, Assert._
import scala.concurrent.duration._
import scala.util.Try
import scuff.Numbers

class TestFailureBackoff {

  def fibonnaciBackoff(maxBackoff: FiniteDuration) =
    Numbers.fibonacci.view.dropWhile(_ < 2).map(_.seconds).takeWhile(_ <= maxBackoff)

  private var errors: List[Throwable] = null

  @Before
  def setup() = errors = Nil

  def reportError(th: Throwable): Unit = errors = th :: errors

  @Test(expected = classOf[IllegalArgumentException])
  def `no backoff schedule`(): Unit = {
    val ft = new FailureTracker(3, reportError, Nil)
    fail(s"Should have failed: $ft")
  }

  @Test(expected = classOf[IllegalArgumentException])
  def `zero failure count threshold`(): Unit = {
    val ft = new FailureTracker(0, reportError, List(2.minutes))
    fail(s"Should have failed: $ft")
  }

  @Test
  def `finite backoff schedule`(): Unit = {
    val error = Try(sys.error("oh no")).failed.get
    val threshold = 3
    val backoffSchedule = fibonnaciBackoff(2.minutes)
    val scheduleLength = backoffSchedule.size

    val ft = new FailureTracker(threshold, reportError, backoffSchedule)
    assertEquals(0, ft.failureCount)
    assertFalse(ft.isTripped)
    assertEquals(Duration.Zero, ft.timeout)

    val backoffIterator = backoffSchedule.iterator
    var currTimeout = backoffIterator.next()

    (1 to scheduleLength + 5) foreach { n =>
      ft reportFailure error
      assertEquals(n, ft.failureCount)
      if (ft.failureCount < threshold) {
        assertFalse(ft.isTripped)
        assertEquals(Duration.Zero, ft.timeout)
      } else {
        assertTrue(ft.isTripped)
        assertEquals(currTimeout, ft.timeout)
        if (backoffIterator.hasNext) currTimeout = backoffIterator.next()
      }
    }
    assertEquals(threshold, errors.size)
    errors.foreach { err =>
      assertSame(error, err)
    }
    ft.reset()
    assertEquals(0, ft.failureCount)
    assertFalse(ft.isTripped)
    assertEquals(Duration.Zero, ft.timeout)

  }

}
