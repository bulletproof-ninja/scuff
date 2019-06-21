package scuff.concurrent

import org.junit._, Assert._
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration._
import java.util.concurrent.Executor
import java.util.concurrent.atomic.AtomicInteger

class TestResourcePool {

  val DefaultScheduler = {
    val tf = Threads.factory("scheduler", _.printStackTrace)
    Threads.newScheduledThreadPool(1, tf, _.printStackTrace)
  }

  val created = new AtomicInteger
  val closed = new AtomicInteger

  @Before
  def reset(): Unit = {
    created.set(0)
    closed.set(0)
  }

  @Test
  def `fail heating when too cold`(): Unit = {
    val pool = new SomeResourcePool
    try {
      pool.startHeater(7.millis)(5.millis, DefaultScheduler) { r =>
        r.touch = System.currentTimeMillis()
      }
      fail("Should fail, since heater can never run")
    } catch {
      case e: IllegalArgumentException => assertTrue(e.getMessage.contains("7"))
    }
    try {
      pool.startHeater(5.millis)(5.millis, DefaultScheduler) { r =>
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
    val testExpirationSchedule = DefaultScheduler.scheduleAtFixedRate(100.millis, 100.millis) {
      val now = System.currentTimeMillis
      if (now - touch > 100) {
        println(s"Touched at $touch, now is $now, (diff ${now - touch} ms), so closing")
        close()
      }
    }
    def close(): Unit = {
      closed.incrementAndGet()
      testExpirationSchedule.cancel(false)
      expired.set(true)
    }
  }
  implicit val lifecycle = ResourcePool
    .onCheckoutReturn[SomeResource](_.touch = System.currentTimeMillis, _.touch = System.currentTimeMillis)
  class SomeResourcePool extends UnboundedResourcePool(new SomeResource)

  private def keepAlive(exe: Executor): Unit = {
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
  def `keep alive w/scheduler`(): Unit = {
    keepAlive(DefaultScheduler)
  }
  @Test
  def `keep alive w/single thread`(): Unit = {
    keepAlive(Threads.newSingleRunExecutor(Threads.factory("Heater", _.printStackTrace), _.printStackTrace))
  }

  @Test(expected = classOf[IllegalArgumentException])
  def `bounded pool min > max`(): Unit = {
    val pool = new BoundedResourcePool(new SomeResource, 3, 2)
    fail(s"Should have failed: $pool")
  }

  @Test(expected = classOf[IllegalArgumentException])
  def `bounded pool max = 0`(): Unit = {
    val pool = new BoundedResourcePool(new SomeResource, 0, 0)
    fail(s"Should have failed: $pool")
  }

  @Test
  def `bounded pool`(): Unit = {
    val pool = new BoundedResourcePool(new SomeResource, 2, 4)
    assertEquals(2, pool.availableCount)
    assertEquals(2, pool.activeCount)
    pool.use { _ =>
      assertEquals(1, pool.availableCount)
      assertEquals(2, pool.activeCount)
      pool.use { _ =>
        assertEquals(0, pool.availableCount)
        assertEquals(2, pool.activeCount)
        pool.use { _ =>
          assertEquals(0, pool.availableCount)
          assertEquals(3, pool.activeCount)
          pool.use { _ =>
            assertEquals(0, pool.availableCount)
            assertEquals(4, pool.activeCount)
            try pool.use(_ => ()) catch {
              case ResourcePool.Exhausted(max, _) => assertEquals(4, max)
            }
            assertEquals(0, pool.availableCount)
            assertEquals(4, pool.activeCount)
          }
          assertEquals(1, pool.availableCount)
          assertEquals(4, pool.activeCount)
        }
        assertEquals(2, pool.availableCount)
        assertEquals(4, pool.activeCount)
      }
      assertEquals(3, pool.availableCount)
      assertEquals(4, pool.activeCount)
    }
    assertEquals(4, pool.availableCount)
    assertEquals(4, pool.activeCount)
    try pool.use(_ => ???) catch {
      case _: Throwable =>
        assertEquals(3, pool.availableCount)
        assertEquals(3, pool.activeCount)
        pool.use { _ =>
          assertEquals(2, pool.availableCount)
          assertEquals(3, pool.activeCount)
        }
    }
    val drained = pool.drain()
    assertEquals(3, drained.size)
    assertEquals(0, pool.availableCount)
    assertEquals(0, pool.activeCount)
    pool.use { _ =>
      assertEquals(0, pool.availableCount)
      assertEquals(1, pool.activeCount)
    }
  }

}
