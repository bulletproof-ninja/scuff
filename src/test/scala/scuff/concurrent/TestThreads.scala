package scuff.concurrent

import java.util.concurrent.{ CountDownLatch, LinkedBlockingQueue, TimeUnit }
import concurrent.ExecutionContext
import concurrent.duration._
import scala.language.reflectiveCalls
import scala.util.{ Failure, Random, Success }

import org.junit.Assert._
import org.junit.Test

import scuff.ScuffRandom
import scala.concurrent.Future
import java.util.concurrent.TimeoutException
import scala.util.control.NonFatal

class TestThreads extends Serializable {
  @Test
  def foo {
    val tf = Threads.factory("MyThread")
    val latch = new CountDownLatch(1)
    val thread = tf newThread new Runnable {
      def run = latch.countDown()
    }
    assertEquals("MyThread[0]", thread.getName)
    assertEquals("MyThread", thread.getThreadGroup.getName)
    thread.start()
    assertTrue(latch.await(2, TimeUnit.SECONDS))
  }

  @Test
  def javaFutures {
    implicit val ec = ExecutionContext.global
    val rand = new Random
    val futures = (1 to 1500).map { i =>
      val f = new java.util.concurrent.Future[Int] {
        val queue = new LinkedBlockingQueue[Int](1)
        def cancel(now: Boolean) = ???
        def isCancelled() = false
        def isDone(): Boolean = !queue.isEmpty()
        def get() = queue.remove()
        def get(t: Long, tu: TimeUnit): Int = ???
      }
      ec execute new Runnable {
        import language.reflectiveCalls
        override def run = {
          Thread sleep rand.nextInRange(1 to 5)
          f.queue.put(i)
        }
      }
      f.asScala
    }
    val set = new collection.concurrent.TrieMap[Int, Unit]
    val cdl = new CountDownLatch(futures.size)
    futures.foreach { f =>
      f.onComplete {
        case Failure(t) => fail("Future failed")
        case Success(i) =>
          set += i -> Unit
          cdl.countDown()
      }
    }
    assertTrue(cdl.await(5, TimeUnit.SECONDS))
    assertEquals(futures.size, set.size)
  }

  @Test
  def `future timeout` {
    import ExecutionContext.Implicits.global

    try {
      val unit = Future(Thread sleep 111).withTimeout(100.millis).await
      fail("Should not succeeed")
      assertNotNull(unit)
    } catch {
      case NonFatal(th) => assertTrue(th.isInstanceOf[TimeoutException])
    }
    val v = Future { Thread sleep 99; 42 }.withTimeout(111.millis).await
    assertEquals(42, v)
  }

}
