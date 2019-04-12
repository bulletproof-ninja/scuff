package scuff

import org.junit._
import org.junit.Assert._
import scuff.concurrent.PartitionedExecutionContext
import java.util.concurrent.TimeUnit
import scala.concurrent.Future

class TestPubSub {
  import language.implicitConversions

  class Event

  implicit def f2sb[T](f: T => Unit) = new StreamConsumer[T, Unit] {
    def onNext(t: T) = f(t)
    def onError(e: Throwable) = e.printStackTrace()
    def onDone() = Future successful (())
  }

  @Test
  def exceptional(): Unit = {
    val countDown = new java.util.concurrent.CountDownLatch(6)
    val exceptions = collection.concurrent.TrieMap[Throwable, Unit]()
      def errHandler(t: Throwable): Unit = {
        exceptions += t -> (())
        countDown.countDown()
      }
    val subCtx = PartitionedExecutionContext(numThreads = 1, failureReporter = errHandler)
    val pubSub = new PubSub[Event, Event](subCtx)
    val s1 = pubSub.subscribe() { _: Event => throw new RuntimeException }
    val s2 = pubSub.subscribe()((_: Event) => countDown.countDown())
    val s3 = pubSub.subscribe() { _: Event => countDown.countDown() }
    pubSub.publish(new Event)
    pubSub.publish(new Event)
    assertTrue(countDown.await(10, TimeUnit.SECONDS))
    assertEquals(2, exceptions.size)
    s1.cancel(); s2.cancel(); s3.cancel()
  }
}
