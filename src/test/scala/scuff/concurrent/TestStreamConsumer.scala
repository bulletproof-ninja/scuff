package scuff.concurrent

import org.junit._, Assert._
import scala.concurrent.Future
import scala.util._

class TestStreamConsumer {

  implicit def ec = RandomDelayExecutionContext
  private def callMe(n: Int)(consumer: StreamConsumer[Int, _]): Unit = Future {
    var i = 0
    while (i < n) {
      consumer onNext Random.nextInt(Int.MaxValue)
      i += 1
    }
    Future(consumer.onDone)
  }

  @Test
  def toPromise() {
    val callMe100000 = callMe(100000) _
    val futureSum = StreamPromise.fold(BigInt(0), callMe100000) {
      case (sum, int) => sum + int
    }
    assertTrue(futureSum.await > 100000)
  }
  @Test
  def adapter() {
    class Sum extends StreamConsumer[Int, BigInt] {
      var th: Throwable = _
      var sum = BigInt(0)
      def onNext(i: Int) = sum += i
      def onError(th: Throwable) = this.th = th
      def onDone() =
        if (th != null) Future failed th
        else Future successful sum
    }
    val sumAsPromise = StreamPromise(new Sum)
    val futureSum = sumAsPromise.future
    callMe(1000000)(sumAsPromise)
    assertTrue(futureSum.await > 1000000)
  }
}
