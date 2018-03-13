package scuff.concurrent

import org.junit._, Assert._
import scala.concurrent.Future
import scuff.StreamConsumer

class TestStreamConsumer {

  implicit def ec = RandomDelayExecutionContext
  private def callMe(n: Int)(consumer: StreamConsumer[Int, _]): Future[Unit] = Future {
    var i = 0
    while (i < n) {
      consumer onNext i
      i += 1
    }
    consumer.onDone()
  }

  @Test
  def toPromise() {
    val callMe100000 = callMe(1000000) _
    val futureSum = StreamPromise.fold(BigInt(0), callMe100000) {
      case (sum, int) => sum + int
    }
    assertEquals(BigInt(499999500000L), futureSum.await)
  }

  @Test
  def promiseAdapter1() {
    var sum = BigInt(0)
    val sumAsPromise = StreamPromise(sum) { i: Int =>
      sum += i
    }
    val futureSum = sumAsPromise.future
    callMe(1000000)(sumAsPromise)
    assertEquals(BigInt(499999500000L), futureSum.await)
  }

  @Test
  def promiseAdapter2() {
    class Sum extends StreamConsumer[Int, Future[BigInt]] {
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
    assertEquals(BigInt(499999500000L), futureSum.await)
  }

  @Test
  def adapter1() {
    var sum = BigInt(0)
    val Sum: StreamConsumer[Int, Unit] = StreamConsumer(th => throw th)(sum += _)
    callMe(1000000)(Sum).await
    assertEquals(BigInt(499999500000L), sum)
  }

  @Test
  def `no adapter`() {
    object Sum extends StreamConsumer[Int, Unit] {
      var sum = BigInt(0)
      def onNext(i: Int) = sum += i
      def onError(th: Throwable) = throw th
      def onDone() = ()
    }
    callMe(1000000)(Sum).await
    assertEquals(BigInt(499999500000L), Sum.sum)
  }
}
