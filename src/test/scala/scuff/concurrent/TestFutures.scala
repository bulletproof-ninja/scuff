package scuff.concurrent

import org.junit._, Assert._
import scala.concurrent._, duration._

class TestFutures {
  @Test
  def futureTimeoutReporting() {
    import concurrent.ExecutionContext.Implicits.global

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
      case timeout: TimeoutException => assertTrue(exception.isEmpty)
    }
    val yes = future2.await(20.millis, caught)
    assertEquals("Oh yeah", yes)
    Thread sleep 201
    exception match {
      case None => fail("Should have exception")
      case Some(e) => assertEquals("Oh noes", e.getMessage)
    }
  }
}
