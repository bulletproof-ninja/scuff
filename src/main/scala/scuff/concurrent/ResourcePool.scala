package scuff.concurrent

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.util.Try
import scala.concurrent.duration.Duration
import java.util.concurrent.Executor
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.ExecutionContext
import java.util.concurrent.ScheduledFuture
import java.util.concurrent.CountDownLatch
import java.util.concurrent.CancellationException
import java.util.concurrent.TimeoutException
import java.util.concurrent.Delayed

/**
  * Unbounded lock-free resource pool.
  *
  * Unlike traditional resource pools, the pool has
  * no upper limit on resources being created, so be careful
  * if that is a concern. This is done to avoid any locking
  * (or spinning) penalty.
  *
  * Any resource is deliberately discarded when
  * an exception occurs, to avoid potentially
  * corrupted resources. This behavior can be changed
  * on a case-by-case basis by sub-classing and
  * overriding `use` and preventing non-destructive
  * exceptions from reaching `super.use`.
  *
  * NOTICE: As with any pool, make absolutely sure the
  * resource does not escape the `use` scope, but
  * that almost goes without saying, amirite?
  */
class ResourcePool[R](newResource: => R, minResources: Int = 0) {

  require(minResources >= 0, s"Cannot have less resources than zero: $minResources")

  private val pool = {
    val initialResources = (0 until minResources).map(_ => 0L -> newResource).toList
    new AtomicReference[List[(Long, R)]](initialResources)
  }

  private def schedulePruning(exec: ScheduledExecutorService, pruner: Pruner) = {
    exec.scheduleWithFixedDelay(pruner, pruner.delayMillis, pruner.intervalMillis, TimeUnit.MILLISECONDS).asInstanceOf[ScheduledFuture[Nothing]]
  }

  private def startPruningThread(exec: Executor, pruner: Pruner): ScheduledFuture[Nothing] = {
    assert(!exec.isInstanceOf[ScheduledExecutorService])
    val cancelled = new CountDownLatch(1)
    val thread = new AtomicReference[Thread]
    object schedule extends ScheduledFuture[Nothing] {
      def cancel(interrupt: Boolean): Boolean = {
        val cancel = cancelled.getCount != 0
        cancelled.countDown()
        if (interrupt) {
          thread.get match {
            case null => // Ignore
            case thr =>
              thread.compareAndSet(thr, null)
              thr.interrupt()
          }
        }
        cancel
      }
      def isCancelled(): Boolean = cancelled.getCount == 0L
      def isDone(): Boolean = isCancelled
      def get(): Nothing = {
        cancelled.await()
        throw new CancellationException
      }
      def get(time: Long, unit: TimeUnit): Nothing = {
        if (cancelled.await(time, unit)) {
          throw new CancellationException
        } else {
          throw new TimeoutException
        }
      }
      def getDelay(unit: TimeUnit): Long = ???
      def compareTo(that: Delayed): Int = ???
    }
    exec execute new Runnable {
      def run {
        Thread.sleep(pruner.delayMillis) // Initial sleep
        while (cancelled.getCount != 0L) {
          try {
            pruner.run()
          } catch {
            case e: Exception => exec match {
              case exeCtx: ExecutionContext => exeCtx.reportFailure(e)
              case _ => e.printStackTrace(System.err)
            }
          }
          cancelled.await(pruner.intervalMillis, TimeUnit.MILLISECONDS)
        }
      }
    }
    schedule
  }

  private final class Pruner(timeoutMillis: Long, cleanup: R => Unit) extends Runnable {
    require(timeoutMillis > 0, "Timeout must be > 0 milliseconds")

    def delayMillis = timeoutMillis * 2
    val intervalMillis = (timeoutMillis / 4).max(1)

    def run = pruneTail()

    @tailrec
    def pruneTail(now: Long = System.currentTimeMillis) {
      pruneLast(now) match {
        case Some(pruned) =>
          Try(cleanup(pruned)) // Best effort cleanup
          pruneTail(now)
        case _ => // Stop
      }
    }

    @tailrec
    def pruneLast(now: Long): Option[R] = {
      pool.get match {
        case poolList if minResources == 0 || poolList.size > minResources =>
          poolList.reverse match {
            case (lastUsed, pruned) :: remaining if lastUsed + timeoutMillis < now =>
              if (pool.compareAndSet(poolList, remaining.reverse)) {
                Some(pruned)
              } else {
                pruneLast(now)
              }
            case _ => None
          }
        case _ => None
      }
    }
  }

  /**
    * Start a thread to prune resources that have
    * not been used for at least the given minimum
    * timeout.
    * @param minimumTimeout The minimum amount of time a resource has been unused before being eligible for pruning.
    * @param destructor Optional resource pruning function.
    * @param executor Scheduler or thread on which to run pruning.
    */
  def startPruning(
    minimumTimeout: FiniteDuration,
    destructor: R => Unit = _ => Unit,
    executor: Executor = Threads.DefaultScheduler): ScheduledFuture[Nothing] = {
    val pruner = new Pruner(minimumTimeout.toMillis, destructor)
    executor match {
      case scheduler: ScheduledExecutorService => schedulePruning(scheduler, pruner)
      case _ => startPruningThread(executor, pruner)
    }
  }

  def size = pool.get.size

  /** Drain pool of all resources. */
  def drain() = pool.getAndSet(Nil).map(_._2)

  @tailrec
  final def pop(): R = {
    pool.get match {
      case Nil =>
        newResource match {
          case null => throw new IllegalStateException("Resource constructor returned `null`.")
          case r =>
            onCheckout(r)
            r
        }
      case list @ (_, head) :: tail =>
        if (pool.weakCompareAndSet(list, tail)) {
          onCheckout(head)
          head
        } else {
          pop()
        }
    }
  }
  final def push(r: R) {
      @tailrec
      def pushUntilSuccessful(time: Long) {
        val list = pool.get
        if (!pool.weakCompareAndSet(list, (time, r) :: list)) {
          pushUntilSuccessful(time)
        }
      }
    onReturn(r)
    pushUntilSuccessful(System.currentTimeMillis)
  }

  /**
    * Called before a resource is
    * borrowed from the pool.
    */
  protected def onCheckout(r: R) {}
  /**
    * Called before a resource is
    * returned to the pool.
    */
  protected def onReturn(r: R) {}

  def use[A](thunk: R => A): A = {
    val r = pop()
    val a = thunk(r)
    push(r)
    a
  }

}
