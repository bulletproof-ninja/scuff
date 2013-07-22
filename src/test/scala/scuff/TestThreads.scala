package scuff

import org.junit._
import org.junit.Assert._
import java.util.concurrent._

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

}
