package scuff.concurrent

import org.junit._, Assert._
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration._
import java.util.concurrent.Executor
import java.util.concurrent.atomic.AtomicInteger

class TestResourcePool {

  val created = new AtomicInteger
  val closed = new AtomicInteger

  @Before
  def reset() {
    created.set(0)
    closed.set(0)
  }

  @Test
  def `fail heating when too cold`() {
    val pool = new SomeResourcePool
    try {
      pool.startHeater(7.millis)(5.millis) { r =>
        r.touch = System.currentTimeMillis()
      }
      fail("Should fail, since heater can never run")
    } catch {
      case e: IllegalArgumentException => assertTrue(e.getMessage.contains("7"))
    }
    try {
      pool.startHeater(5.millis)(5.millis) { r =>
        r.touch = System.currentTimeMillis()
      }
      fail("Should fail, since heater can never run")
    } catch {
      case e: IllegalArgumentException => assertTrue(e.getMessage.contains("5"))
    }
  }

  class SomeResource {
    created.incrementAndGet()
    @volatile var touch = System.currentTimeMillis
    private val expired = new AtomicBoolean(false)
    def isExpired = expired.get
    val testExpirationSchedule = Threads.DefaultScheduler.scheduleAtFixedRate(100.millis, 100.millis) {
      val now = System.currentTimeMillis
      if (now - touch > 100) {
        println(s"Touched at $touch, now is $now, (diff ${now - touch} ms), so closing")
        close()
      }
    }
    def close() {
      closed.incrementAndGet()
      testExpirationSchedule.cancel(false)
      expired.set(true)
    }
  }
  class SomeResourcePool extends ResourcePool(new SomeResource) {
    override def onCheckout(r: SomeResource) = {
      r.touch = System.currentTimeMillis()
    }
    override def onReturn(r: SomeResource) {
      r.touch = System.currentTimeMillis()
    }
  }

  private def keepAlive(exe: Executor) {
    val pool = new SomeResourcePool
    val schedule = pool.startHeater()(50.millis, exe) { r =>
      r.touch = System.currentTimeMillis()
    }
    Thread sleep 1
    pool.use { r1 =>
      pool.use { r2 =>
        pool.use { r3 =>
          println((r1.touch, r2.touch, r3.touch))
        }
      }
    }
    Thread sleep 1
    assertEquals(3, created.get)
    assertEquals(0, closed.get)
    Thread sleep 200
    assertEquals(3, created.get)
    assertEquals(0, closed.get)
    pool.use { r1 =>
      pool.use { r2 =>
        pool.use { r3 =>
          println((r1.touch, r2.touch, r3.touch))
        }
      }
    }
    assertTrue(created.get >= 3)
    assertEquals(0, closed.get)
    for (ms <- 10 to 200 by 10) {
      Thread sleep ms
      val r = pool.pop()
      try assertFalse(r.isExpired) finally pool.push(r)
      assertTrue(created.get >= 3)
      assertEquals(0, closed.get)
    }
    pool.drain().foreach(_.close())
    assertEquals(created.get, closed.get)
    schedule.cancel(false)
  }

  @Test
  def `keep alive w/scheduler`() {
    keepAlive(Threads.DefaultScheduler)
  }
  @Test
  def `keep alive w/single thread`() {
    keepAlive(Threads.newSingleRunExecutor(Threads.factory("Heater")))
  }
}
