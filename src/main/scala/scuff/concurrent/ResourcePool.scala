package scuff.concurrent

import java.util.concurrent.{ CancellationException, CountDownLatch, Delayed, Executor, ScheduledExecutorService, ScheduledFuture, TimeUnit, TimeoutException }
import java.util.concurrent.atomic.AtomicReference

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.util.Try
import scala.util.control.NonFatal
import scala.concurrent.duration.Duration

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
class ResourcePool[R <: AnyRef](newResource: => R, minResources: Int = 0) {

  require(minResources >= 0, s"Cannot have less resources than zero: $minResources")

  @inline
  private def currentMillis = System.currentTimeMillis

  private val pool = {
    val initialResources = (0 until minResources).map(_ => 0L -> newResource).toList
    new AtomicReference[List[(Long, R)]](initialResources)
  }

  private def schedule(exec: ScheduledExecutorService, r: Runnable, delay: FiniteDuration, interval: FiniteDuration) = {
    exec.scheduleWithFixedDelay(r, delay.toMillis, interval.toMillis, TimeUnit.MILLISECONDS)
      .asInstanceOf[ScheduledFuture[Nothing]]
  }

  private class Schedule(
      thread: AtomicReference[Thread],
      cancelled: CountDownLatch) extends ScheduledFuture[Nothing] {
    def cancel(interrupt: Boolean): Boolean = {
      val cancel = cancelled.getCount != 0
      cancelled.countDown()
      if (interrupt) {
        thread.get match {
          case null => // Ignore
          case thr =>
            thread.weakCompareAndSet(thr, null)
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

  private def startThread(exec: Executor, r: Runnable, delay: FiniteDuration, interval: FiniteDuration): Schedule = {
    assert(!exec.isInstanceOf[ScheduledExecutorService])
    val thread = new AtomicReference[Thread]
    val cancelled = new CountDownLatch(1)
    val schedule = new Schedule(thread, cancelled)
    exec execute new Runnable {
      def run {
        Thread.sleep(delay.toMillis) // Initial sleep
        while (cancelled.getCount != 0L) {
          try {
            r.run()
          } catch {
            case e: Exception => exec match {
              case exeCtx: ExecutionContext => exeCtx.reportFailure(e)
              case _ => e.printStackTrace(System.err)
            }
          }
          cancelled.await(interval.toMillis, TimeUnit.MILLISECONDS)
        }
      }
    }
    schedule
  }

  private final class Pruner(timeoutMillis: Long, cleanup: R => Unit) extends Runnable {
    require(timeoutMillis > 0, "Timeout must be > 0 milliseconds")

    def delay = new FiniteDuration(timeoutMillis * 2, TimeUnit.MILLISECONDS)
    def interval = new FiniteDuration((timeoutMillis / 4) max 1, TimeUnit.MILLISECONDS)

    def run = pruneTail()

    @tailrec
    def pruneTail(now: Long = currentMillis) {
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
        case poolList if minResources == 0 || poolList.drop(minResources).nonEmpty =>
          poolList.reverse match {
            case (lastUsed, pruned) :: remaining if lastUsed + timeoutMillis < now =>
              if (pool.weakCompareAndSet(poolList, remaining.reverse)) {
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

  private final class Heater(heater: R => Unit, excludeHottestMillis: Long) extends Runnable {

    private def safeHeat(cool: R): Boolean = try { heater(cool); true } catch {
      case NonFatal(e) => false
    }

    @annotation.tailrec
    private def heatPool() {
      val now = currentMillis
      val poolList = pool.get
        def isHot(t: (Long, R)): Boolean = now - t._1 < excludeHottestMillis
      val hot = poolList.takeWhile(isHot)
      val cool = poolList.dropWhile(isHot)
      if (cool.nonEmpty) {
        if (!pool.weakCompareAndSet(poolList, hot)) {
          heatPool()
        } else {
          val reheated = cool flatMap {
            case (_, resource) =>
              if (safeHeat(resource)) new Some(currentMillis -> resource)
              else None
          }
          pushUntilSuccessful(reheated)
        }
      }
    }

    def run = heatPool()
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
    destructor: R => Unit = Function const (()),
    executor: Executor = Threads.DefaultScheduler): ScheduledFuture[Nothing] = {
    val pruner = new Pruner(minimumTimeout.toMillis, destructor)
    executor match {
      case scheduler: ScheduledExecutorService => schedule(scheduler, pruner, pruner.delay, pruner.interval)
      case _ => startThread(executor, pruner, pruner.delay, pruner.interval)
    }
  }

  /**
    *  Keep resources hot.
    *  @param excludeHottest Exclude if resource has been used within the given duration.
    *  Use zero to include all. Must be less than interval.
    *  @param interval The interval the heater is invoked
    *  @param executor The thread running the heater. If not a [[ScheduledExecutorService]], a
    *  single thread will be monopolized entirely, until cancelled.
    */
  def startHeater(
    excludeHottest: FiniteDuration = Duration.Zero)(
      interval: FiniteDuration,
      executor: Executor = Threads.DefaultScheduler)(heater: R => Unit): ScheduledFuture[Nothing] = {

    require(interval.length > 0, s"Must have interval: $interval")
    require(
      excludeHottest < interval,
      s"Heater is running every $interval, thus excluding all used within $excludeHottest will effectively disable heater")

    val runHeater = new Heater(heater, excludeHottest.toMillis)
    executor match {
      case scheduler: ScheduledExecutorService => schedule(scheduler, runHeater, interval, interval)
      case _ => startThread(executor, runHeater, interval, interval)
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

  private implicit val ordering = Ordering.by[(Long, R), Long](_._1).reverse
  @tailrec
  private def pushUntilSuccessful(append: List[(Long, R)]) {
    val list = pool.get
    val sorted = (list ++ append).sorted
    if (!pool.weakCompareAndSet(list, sorted)) {
      pushUntilSuccessful(append)
    }
  }

  final def push(r: R) {
    onReturn(r)
    val tuple = currentMillis -> r
    val list = pool.get
    if (!pool.weakCompareAndSet(list, tuple :: list)) {
      pushUntilSuccessful(List(tuple))
    }
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
