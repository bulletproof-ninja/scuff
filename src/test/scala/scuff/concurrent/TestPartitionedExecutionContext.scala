package scuff.concurrent

import org.junit._
import org.junit.Assert._
import scala.concurrent.duration._
import scala.util.Random
import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.LinkedBlockingDeque

class TestPartitionedExecutionContext {

  @annotation.tailrec
  private def updateMap(hash: Int, thread: Thread, map: collection.concurrent.Map[Int, Set[Thread]]): Unit = {
    map.get(hash) match {
      case Some(threadSet) =>
        if (!map.replace(hash, threadSet, threadSet + thread)) {
          updateMap(hash, thread, map)
        }
      case None =>
        if (map.putIfAbsent(hash, Set(thread)).isDefined) {
          updateMap(hash, thread, map)
        }
    }
  }

  @Test
  def verify(): Unit = {
    val threadGroup = Threads.newThreadGroup("verify", false, _.printStackTrace)
    val numThreads = 16

      def verify(newQueue: => BlockingQueue[Runnable], aggregateCapacity: Int = Int.MaxValue): Unit = {
        val queues = Seq.fill(numThreads)(newQueue)
        val ec = PartitionedExecutionContext.apply(queues, aggregateCapacity, threadGroup, Threads.factory(threadGroup), _.hashCode)
        val jobsPerHash = 100
        val hashRange = -5000 to 5000
        val threadsByHash = new LockFreeConcurrentMap[Int, Set[Thread]]
        val futures =  for (_ <- 1 to jobsPerHash; hash <- hashRange) yield {
          val a = Random.nextInt()
          val b = Random.nextInt()
          (a*b) -> ec.submit(hash) {
            updateMap(hash, Thread.currentThread, threadsByHash)
            a * b
          }
        }
        // ec.shutdown().await(60.seconds)
        futures.foreach {
          case (result, future) =>
            val futureResult = future.await(5.seconds)
            assertEquals(result, futureResult)
        }
        var allThreads = Set.empty[Thread]
        threadsByHash.map(_._2).foreach { threadSet =>
          assertTrue(1 == threadSet.size)
          allThreads += threadSet.head
        }
        assertEquals(numThreads, allThreads.size)
      }

    verify(new LinkedBlockingQueue[Runnable])
    verify(new LinkedBlockingQueue[Runnable](10), 10)
    verify(new ArrayBlockingQueue[Runnable](5), 5)
    verify(new ArrayBlockingQueue[Runnable](25), 25)
    try {
      verify(new ArrayBlockingQueue[Runnable](24), 25)
      fail("Should fail on queue capacity being less than aggregate queue capacity")
    } catch {
      case e: IllegalArgumentException =>
        assertTrue(e.getMessage contains "25")
    }
    verify(new LinkedBlockingDeque[Runnable](25), 25)
    verify(new LinkedBlockingDeque[Runnable], 25)
    verify(new LinkedBlockingDeque[Runnable])

  }
}
