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

  class SomeResource {
    created.incrementAndGet()
    @volatile var touch = System.currentTimeMillis
    private val expired = new AtomicBoolean(false)
    def isExpired = expired.get
    val testExpirationSchedule = Threads.DefaultScheduler.scheduleAtFixedRate(10.millis, 10.millis) {
      if (System.currentTimeMillis - touch > 10) {
        close()
      }
    }
    def close() {
      closed.incrementAndGet()
      testExpirationSchedule.cancel(false)
      expired.set(true)
    }
  }

  private def keepAlive(exe: Executor) {
    val pool = new ResourcePool(new SomeResource) {
      override def onCheckout(r: SomeResource) = {
        r.touch = System.currentTimeMillis()
      }
      override def onReturn(r: SomeResource) {
        r.touch = System.currentTimeMillis()
      }
    }
    pool.startHeater(5.millis, exe) { r =>
      r.touch = System.currentTimeMillis()
    }
    pool.use { r1 =>
      pool.use { r2 =>
        pool.use { r3 =>
          println((r1.touch, r2.touch, r3.touch))
        }
      }
    }
    assertEquals(3, created.get)
    assertEquals(0, closed.get)
    Thread sleep 20
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
    for (ms <- 5 to 50) {
      Thread sleep ms
      val r = pool.pop()
      try assertFalse(r.isExpired) finally pool.push(r)
      assertTrue(created.get >= 3)
      assertEquals(0, closed.get)
    }
    pool.drain().foreach(_.close())
    assertEquals(created.get, closed.get)
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
