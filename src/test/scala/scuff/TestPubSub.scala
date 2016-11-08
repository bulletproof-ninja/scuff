package scuff

import org.junit._
import org.junit.Assert._
import scala.concurrent.ExecutionContext
import scuff.concurrent.PartitionedExecutionContext
import scuff.concurrent.StreamCallback
import java.util.concurrent.TimeUnit

class TestPubSub {

  class Event

  implicit def f2sb[T](f: T => Unit) = new StreamCallback[T] {
    def onNext(t: T) = f(t)
    def onError(e: Throwable) = e.printStackTrace()
    def onCompleted() = ()
  }

  @Test @Ignore
  def exceptional {
    val countDown = new java.util.concurrent.CountDownLatch(6)
    val exceptions = collection.concurrent.TrieMap[Throwable, Unit]()
      def errHandler(t: Throwable) {
        exceptions += t -> Unit
        countDown.countDown()
      }
    val execCtx = PartitionedExecutionContext(numThreads = 2, failureReporter = errHandler)
    val pubSub = new PubSub[Event, Event](execCtx)
    val s1 = pubSub.subscribe() { e: Event => throw new RuntimeException }
    val s2 = pubSub.subscribe()((e: Event) => countDown.countDown())
    val s3 = pubSub.subscribe() { e: Event => countDown.countDown() }
    pubSub.publish(new Event)
    pubSub.publish(new Event)
    assertTrue(countDown.await(5, TimeUnit.SECONDS))
    assertEquals(2, exceptions.size)
    s1.cancel(); s2.cancel(); s3.cancel()
  }
}
