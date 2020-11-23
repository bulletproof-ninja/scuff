package scuff.concurrent

import org.junit._, Assert._
import scala.concurrent.Future
import scala.concurrent.duration._
import scuff.StreamConsumer
import java.util.concurrent.atomic.AtomicInteger
import scala.util.Try
import scala.util.Failure
import scala.concurrent.ExecutionContext

object TestStreamConsumer {
  def main(args: Array[String]): Unit = {
    new TestStreamConsumer().`async, onstack failure in apply`()
  }
}

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
  def toPromise(): Unit = {
    val callMe100000 = callMe(1000000) _
    val futureSum = StreamPromise.fold(BigInt(0), callMe100000) {
      case (sum, int) => sum + int
    }
    assertEquals(BigInt(499999500000L), futureSum.await)
  }

  @Test
  def promiseAdapter1(): Unit = {
    var sum = BigInt(0)
    val sumAsPromise = StreamPromise(sum) { i: Int =>
      sum += i
    }
    val futureSum = sumAsPromise.future
    callMe(1000000)(sumAsPromise)
    assertEquals(BigInt(499999500000L), futureSum.await)
  }

  @Test
  def promiseAdapter2(): Unit = {
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
  def adapter1(): Unit = {
    var sum = BigInt(0)
    val Sum: StreamConsumer[Int, Unit] = StreamConsumer(th => throw th)(sum += _)
    callMe(1000000)(Sum).await
    assertEquals(BigInt(499999500000L), sum)
  }

  @Test
  def `no adapter`(): Unit = {
    object Sum extends StreamConsumer[Int, Unit] {
      var sum = BigInt(0)
      def onNext(i: Int) = sum += i
      def onError(th: Throwable) = throw th
      def onDone() = ()
    }
    callMe(1000000)(Sum).await
    assertEquals(BigInt(499999500000L), Sum.sum)
  }

  @Test
  def `async, success`(): Unit = {
    object Average extends StrictAsyncStreamConsumer[Int, Int] with (Int => Future[Unit]) {

      override def activeCount = super.activeCount
      override def totalCount = super.totalCount

      protected def executionContext = ExecutionContext.global

      private val sum = new AtomicInteger
      private val count = new AtomicInteger
      def apply(i: Int) = {
        sum.addAndGet(i)
        count.incrementAndGet()
        Future.unit
      }
      val completionTimeout = 5.seconds
      protected def whenDone(): Future[Int] = Future fromTry Try(sum.get / count.get)
    }
    assertEquals(0, Average.activeCount)
    assertEquals(0, Average.totalCount)

    (0 to 100).foreach(Average.onNext)
    assertEquals(101, Average.totalCount)
    val result = Average.onDone().await
    assertEquals(50, result)
  }

  @Test
  def `async, future failure in apply`(): Unit = {
    object Average extends StrictAsyncStreamConsumer[Int, Int] with (Int => Future[Unit]) {
      protected def executionContext = ExecutionContext.global
      def apply(i: Int) = Future failed new IllegalArgumentException(s"Invalid number: $i")
      val completionTimeout = 5.seconds
      protected def whenDone(): Future[Int] = Future successful 42
    }
    (0 to 100).foreach(Average.onNext)
    Try(Average.onDone().await) match {
      case Failure(e: IllegalArgumentException) => assertTrue(e.getMessage contains "Invalid number:")
      case other => fail(s"Should have failed on IllegalArgumentException, was $other")
    }
  }

  @Test
  def `async, onstack failure in apply`(): Unit = {
    object Average extends StrictAsyncStreamConsumer[Int, Int] with (Int => Future[Unit]) {
      override def totalCount: Long = super.totalCount
      protected def executionContext = ExecutionContext.global
      def apply(i: Int) = throw new IllegalArgumentException(s"Invalid number: $i")
      val completionTimeout = 5.seconds
      protected def whenDone(): Future[Int] = Future successful 42
    }
    (0 to 100).foreach { n =>
      Average onNext n
    }
    assertEquals(101, Average.totalCount)
    Try(Average.onDone().await) match {
      case Failure(e: IllegalArgumentException) =>
        assertTrue(e.getMessage startsWith "Invalid number:")
      case other =>
        fail(s"Should have failed on IllegalArgumentException, was $other")
    }
  }

  @Test
  def `async, failure in onDone`(): Unit = {
    object Average extends StrictAsyncStreamConsumer[Int, Int] with (Int => Future[Unit]) {
      protected def executionContext = ExecutionContext.global
      private val sum = new AtomicInteger
      private val count = new AtomicInteger
      def apply(i: Int) = ??? // Never called in this test
      val completionTimeout = 5.seconds
      protected def whenDone(): Future[Int] = Future fromTry Try(sum.get / count.get)
    }
    //(0 to 100).foreach(Average.onNext)
    Try(Average.onDone().await) match {
      case Failure(e: ArithmeticException) => assertTrue(e.getMessage contains "zero")
      case _ => fail("Should have failed on division by zero")
    }
  }
}
