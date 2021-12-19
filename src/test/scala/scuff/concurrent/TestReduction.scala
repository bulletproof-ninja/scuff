package scuff.concurrent

import org.junit._, Assert._
import scala.concurrent.Future
import scala.concurrent.duration._
import scuff._
import java.util.concurrent.atomic.AtomicInteger
import scala.util.Try
import scala.util.Failure
import scala.concurrent.ExecutionContext
import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicLong

object TestReduction {
  def main(args: Array[String]): Unit = {
    new TestReduction().promiseAdapter1()
  }
}

class TestReduction {

  implicit def ec = RandomDelayExecutionContext
  private def callMe[R](n: Int)(reduction: Reduction[Int, R]): Future[R] = Future {
    var i = 0
    while (i < n) {
      reduction next i
      i += 1
    }
    reduction.result()
  }

  @Test
  def toPromise(): Unit = {
    val callMe100000 = (callMe[BigInt](1000000) _).asInstanceOf[Function1[Reduction[Int, _], _]]
    val futureSum: Future[BigInt] = ReductionPromise.fold[Int, BigInt](BigInt(0), callMe100000) {
      case (sum, int) => sum + int
    }
    assertEquals(BigInt(499999500000L), futureSum.await)
  }

  @Test
  def promiseAdapter1(): Unit = {
    var sum = BigInt(0)
    val sumAsPromise = ReductionPromise(sum) { i: Int =>
      sum += i
    }
    val futureSum = sumAsPromise.future
    callMe(1000000)(sumAsPromise)
    assertEquals(BigInt(499999500000L), futureSum.await(60.seconds))
  }

  @Test
  def promiseAdapter2(): Unit = {
    class Sum extends Reduction[Int, BigInt] {
      var sum = BigInt(0)
      def next(i: Int) = sum += i
      def result() = sum
    }
    val sumAsPromise = ReductionPromise(new Sum)
    val futureSum = sumAsPromise.future
    callMe(1000000)(sumAsPromise)
    assertEquals(BigInt(499999500000L), futureSum.await)
  }

  @Test
  def `no adapter`(): Unit = {
    object Sum extends ForEach[Int] {
      var sum = BigInt(0)
      def next(i: Int) = sum += i
    }
    callMe(1000000)(Sum).await
    assertEquals(BigInt(499999500000L), Sum.sum)
  }

  @Test
  def `async, success`(): Unit = {
    object Average
    extends ConcurrentReduction[Int, Int]
    with NoFailuresAccepted[Int] {

      override def activeCount = super.activeCount
      override def totalCount = super.totalCount

      protected def executionContext = ExecutionContext.global

      private val sum = new AtomicInteger
      private val count = new AtomicInteger
      def asyncNext(i: Int) = {
        sum.addAndGet(i)
        count.incrementAndGet()
        Future.unit
      }
      val completionTimeout = 5.seconds
      protected def asyncResult(): Future[Int] = Future fromTry Try(sum.get / count.get)
    }
    assertEquals(0, Average.activeCount)
    assertEquals(0, Average.totalCount)

    (0 to 100).foreach(Average.next)
    assertEquals(101, Average.totalCount)
    val result = Average.result().await
    assertEquals(50, result)
  }

  @Test
  def `async, future failure in apply`(): Unit = {
    object Average
    extends ConcurrentReduction[Int, Int]
    with NoFailuresAccepted[Int] {
      protected def executionContext = ExecutionContext.global
      def asyncNext(i: Int) = Future failed new IllegalArgumentException(s"Invalid number: $i")
      val completionTimeout = 5.seconds
      protected def asyncResult(): Future[Int] = Future successful 42
    }
    (0 to 100).foreach(Average.next)
    Try(Average.result().await) match {
      case Failure(e: IllegalArgumentException) => assertTrue(e.getMessage contains "Invalid number:")
      case other => fail(s"Should have failed on IllegalArgumentException, was $other")
    }
  }

  @Test
  def `async, onstack failure in apply`(): Unit = {
    object Average
    extends ConcurrentReduction[Int, Int]
    with NoFailuresAccepted[Int] {
      override def totalCount: Long = super.totalCount
      protected def executionContext = ExecutionContext.global
      def asyncNext(i: Int) = throw new IllegalArgumentException(s"Invalid number: $i")
      val completionTimeout = 5.seconds
      protected def asyncResult(): Future[Int] = Future successful 42
    }
    (0 to 100).foreach { n =>
      Average next n
    }
    assertEquals(101, Average.totalCount)
    Try(Average.result().await) match {
      case Failure(e: IllegalArgumentException) =>
        assertTrue(e.getMessage startsWith "Invalid number:")
      case other =>
        fail(s"Should have failed on IllegalArgumentException, was $other")
    }
  }

  @Test
  def `async, failure in result`(): Unit = {
    object Average
    extends ConcurrentReduction[Int, Int]
    with NoFailuresAccepted[Int] {
      protected def executionContext = ExecutionContext.global
      private val sum = new AtomicInteger
      private val count = new AtomicInteger
      def asyncNext(i: Int) = ??? // Never called in this test
      val completionTimeout = 5.seconds
      protected def asyncResult(): Future[Int] = Future fromTry Try(sum.get / count.get)
    }
    //(0 to 100).foreach(Average.next)
    Try(Average.result().await) match {
      case Failure(e: ArithmeticException) => assertTrue(e.getMessage contains "zero")
      case _ => fail("Should have failed on division by zero")
    }
  }

  @Test
  def `fully async`() = {
    object Sum extends ConcurrentReduction[Long, Long] {
      override def activeCount: Int = super.activeCount
      override def totalCount: Long = super.totalCount
      implicit protected def executionContext: ExecutionContext = RandomDelayExecutionContext
      protected def completionTimeout: FiniteDuration = 5.seconds
      private val sum = new AtomicLong
      def asyncNext(n: Long): Future[_] = Future {
        println(s"S + $n = ${ sum addAndGet n }")
      }
      protected def asyncResult(timedOut: Option[TimeoutException], errors: List[Throwable]): Future[Long] = {
        assertEquals(None, timedOut)
        assertEquals(Nil, errors)
        Future successful sum.get
      }
    }
    (1L to 100L).foreach(Sum.next)
    assertEquals(100L, Sum.totalCount)
    val sum = Sum.result().await
    assertEquals(0, Sum.activeCount)
    assertEquals(5050, sum)
  }


}
