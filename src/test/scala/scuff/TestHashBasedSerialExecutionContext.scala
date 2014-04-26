package scuff

import org.junit._
import org.junit.Assert._
import java.util.concurrent.CountDownLatch
import scala.concurrent.Await
import scala.concurrent.duration._

class TestHashBasedSerialExecutionContext {
  @Test
  def await {
    val aw = HashBasedSerialExecutionContext.global.submit(1)(5*7)
    val result = Await.result(aw, 2.seconds)
    assertEquals(5*7, result)
  }
}