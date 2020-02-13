package scuff.concurrent

import scuff._
import org.junit._, Assert._
import scala.concurrent._, duration._
import scala.util.Random
import java.util.concurrent.atomic.AtomicInteger

class TestFutures {

  @Test
  def futureTimeoutReporting(): Unit = {
    import ExecutionContext.Implicits.global
    val future1: Future[String] = Future {
      Thread sleep 200
      sys.error("Oh noes")
    }
    val future2: Future[String] = Future {
      "Oh yeah"
    }

    @volatile var exception: Option[Throwable] = None
      def caught(th: Throwable) = exception = Some(th)

    try {
      future1.await(2.millis, caught)
      fail("Should have timed out")
    } catch {
      case _: TimeoutException => assertTrue(exception.isEmpty)
    }
    val yes = future2.await(20.millis, caught)
    assertEquals("Oh yeah", yes)
    Thread sleep 201
    exception match {
      case None => fail("Should have exception")
      case Some(e) => assertEquals("Oh noes", e.getMessage)
    }
  }

  @Test
  def boundedActiveFutures(): Unit = {
    val maxActive = 9
    val active = new AtomicInteger
    implicit def ec = RandomDelayExecutionContext
    val range = 1 to 1111
    val futuresIterator = range.iterator.map { n =>
      println(s"Submitting future #$n! (${active.get} total running)")
      Future {
        val act = active.incrementAndGet()
        assert(act <= maxActive)
        println(s"Future #$n running! ($act total running)")
        Thread sleep Random.nextBetween(1, 11)
        println(s"Future #$n done! (${active.decrementAndGet} total running)")
        n
      }
    }
    val lazyBoundedIterator = futuresIterator.limitActive(maxActive)
    val futureSum = (Future sequence lazyBoundedIterator).map(_.sum)
    assertEquals(range.sum, futureSum await 60.seconds)
  }

}
