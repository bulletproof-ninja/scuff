package scuff.concurrent

import java.util.concurrent.{ CountDownLatch, LinkedBlockingQueue, TimeUnit }
import concurrent.duration._
import scala.util.{ Failure, Random, Success }

import org.junit.Assert._
import org.junit.Test

import scuff.ScuffRandom
import scala.concurrent.Future
import java.util.concurrent.TimeoutException
import scala.util.control.NonFatal

class TestThreads extends Serializable {

  implicit val DefaultScheduler = {
    val tf = Threads.factory("scheduler", _.printStackTrace)
    Threads.newScheduledThreadPool(8, tf, _.printStackTrace)
  }

  @Test
  def foo(): Unit = {
    val tf = Threads.factory("MyThread", _.printStackTrace)
    val latch = new CountDownLatch(1)
    val thread = tf newThread new Runnable {
      def run = latch.countDown()
    }
    assertEquals("MyThread.0", thread.getName)
    assertEquals("MyThread", thread.getThreadGroup.getName)
    thread.start()
    assertTrue(latch.await(2, TimeUnit.SECONDS))
  }

  @Test
  def javaFutures(): Unit = {
    implicit val jc = JavaFutureConverter(_.printStackTrace)

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
      DefaultScheduler execute new Runnable {
        override def run = {
          Thread sleep rand.nextBetween(1, 6)
          f.queue.put(i)
        }
      }
      f.asScala
    }
    val set = new collection.concurrent.TrieMap[Int, Unit]
    val cdl = new CountDownLatch(futures.size)
    futures.foreach { f =>
      f.onComplete {
        case Failure(_) => fail("Future failed")
        case Success(i) =>
          set += i -> (())
          cdl.countDown()
      }
    }
    assertTrue(cdl.await(5, TimeUnit.SECONDS))
    assertEquals(futures.size, set.size)
  }

  @Test
  def `future timeout`(): Unit = {
    try {
      val unit = Future(Thread sleep 1111).withTimeout(55.millis).await
      fail("Should not succeeed")
      assertNotNull(unit)
    } catch {
      case NonFatal(th) => assertTrue(th.isInstanceOf[TimeoutException])
    }
    val v = Future { Thread sleep 55; 42 }.withTimeout(1111.millis).await
    assertEquals(42, v)
  }

  @Test
  def scheduler_schedule(): Unit = {
    val cdl = new CountDownLatch(1)
    val scheduled = DefaultScheduler.schedule(200.milliseconds)(cdl.countDown)
    assertFalse(scheduled.isDone)
    assertTrue(cdl.await(5, TimeUnit.SECONDS))
    Thread sleep 10 // Sometimes the cdl is triggered so fast, the future is not yet done.
    assertTrue(scheduled.isDone)
  }
  @Test
  def scheduler_fixedRate(): Unit = {
    val cdl = new CountDownLatch(5)
    val scheduled = DefaultScheduler.scheduleAtFixedRate(200.milliseconds, 10000.microseconds)(cdl.countDown)
    assertFalse(scheduled.isDone)
    assertTrue(cdl.await(5, TimeUnit.SECONDS))
    scheduled.cancel(true)
    Thread sleep 10 // Sometimes the cdl is triggered so fast, the future is not yet done.
    assertTrue(scheduled.isDone)
  }
  @Test
  def scheduler_fixedDelay(): Unit = {
    val cdl = new CountDownLatch(5)
    val scheduled = DefaultScheduler.scheduleWithFixedDelay(200.milliseconds, 10000.microseconds)(cdl.countDown)
    assertFalse(scheduled.isDone)
    assertTrue(cdl.await(5, TimeUnit.SECONDS))
    scheduled.cancel(true)
    Thread sleep 10 // Sometimes the cdl is triggered so fast, the future is not yet done.
    assertTrue(scheduled.isDone)
  }

  @Test
  def little_piggy(): Unit = {
    val result = Threads.onBlockingThread("testing") {
      println(s"Sleeping on: ${Thread.currentThread}")
      Thread sleep 100
      "Hello"
    }
    val helloWorld = result.map  { hello =>
      println(s"Future.map on: ${Thread.currentThread}")
      s"$hello, World!"
    }(Threads.PiggyBack)
    assertEquals("Hello, World!", helloWorld.await)
    println(s"Finishing method on: ${Thread.currentThread}")
  }

  @Test
  def has_name(): Unit = {
    val name = "tHeNaMe"
    val pool = Threads.newBlockingThreadPool(name, throw _)
    println(s"Pool name: $pool")
    assertTrue(pool.toString contains name)
    val threadName = pool.submit {
      Thread.currentThread.getName
    }
    assertTrue(threadName.await contains name)
  }

}
