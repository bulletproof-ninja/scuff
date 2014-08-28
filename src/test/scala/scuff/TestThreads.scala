package scuff

import org.junit._
import org.junit.Assert._
import java.util.concurrent._
import scala.util.Random
import scala.util.Failure
import scala.util.Success
import concurrent.blocking

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
    implicit val ec = concurrent.ExecutionContext.global
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

}
