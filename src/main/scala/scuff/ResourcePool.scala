package scuff

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.util.Try
import scala.concurrent.duration.Duration
import java.util.concurrent.Executor
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.TimeUnit

/**
 * Unbounded lock-free resource pool.
 *
 * This pool can be used as a more efficient replacement
 * for [[ThreadLocal]], in that it will never create more
 * instances than there are threads, like [[ThreadLocal]],
 * but has much higher probability for creating less.
 *
 * Any resource is deliberately discarded when
 * an exception occurs, to avoid potentially
 * corrupted resources. This behavior can easily
 * be changed on a case-by-case basis by sub-classing
 * and overriding `borrow` and preventing non-destructive
 * exceptions from reaching `super.borrow`.
 *
 * NOTICE: As with any pool, make absolutely sure the
 * resource does not escape the `borrow` scope.
 */
class ResourcePool[R](constructor: ⇒ R, minResources: Int = 0) {

  require(minResources >= 0, s"Cannot have less resources than zero: $minResources")

  private val pool = {
    val initialResources = (0 until minResources).map(_ => 0L -> constructor).toList
    new AtomicReference[List[(Long, R)]](initialResources)
  }

  private def schedulePruning(exec: ScheduledExecutorService, pruner: Pruner) {
    exec.scheduleWithFixedDelay(pruner, pruner.delayMillis, pruner.intervalMillis, TimeUnit.MILLISECONDS)
  }

  private def startPruningThread(exec: Executor, pruner: Pruner) {
    assert(!exec.isInstanceOf[ScheduledExecutorService])
    exec execute new Runnable {
      def run {
        Thread.sleep(pruner.delayMillis) // Initial sleep
        while (!Thread.currentThread.isInterrupted) {
          Thread.sleep(pruner.intervalMillis)
        }
      }
    }
  }

  private final class Pruner(timeoutMillis: Long, destructor: R => Unit) extends Runnable {
    require(timeoutMillis > 0, "Timeout must be > 0 milliseconds")

    def delayMillis = timeoutMillis * 2
    val intervalMillis = (timeoutMillis / 4).max(1)

    def run = pruneTail()

    @tailrec
    def pruneTail(now: Long = System.currentTimeMillis) {
      pruneLast(now) match {
        case Some(pruned) =>
          destructor(pruned)
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
  def startPruning(minimumTimeout: Duration, destructor: R ⇒ Unit = _ ⇒ Unit, executor: Executor = Threads.DefaultScheduler) {
    require(minimumTimeout.isFinite, "Timeout must be a finite duration")
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
  private def pop(): R = {
    pool.get match {
      case Nil ⇒
        constructor match {
          case null ⇒ throw new IllegalStateException("Resource constructor returned `null`.")
          case r ⇒
            onCheckout(r)
            r
        }
      case list @ (_, head) :: tail ⇒
        if (pool.compareAndSet(list, tail)) {
          onCheckout(head)
          head
        } else {
          pop()
        }
    }
  }
  private def push(r: R, time: Long = System.currentTimeMillis) {
      @tailrec
      def pushUntilSuccessful() {
        val list = pool.get
        if (!pool.compareAndSet(list, (time, r) :: list)) {
          pushUntilSuccessful()
        }
      }
    onReturn(r)
    pushUntilSuccessful()
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

  def borrow[A](thunk: R ⇒ A): A = {
    val r = pop()
    val a = thunk(r)
    push(r)
    a
  }

}

private object ResourcePool {
  lazy val ThreadGroup = new ThreadGroup(Threads.SystemThreadGroup, classOf[ResourcePool[_]].getName)
  //  def ThreadFactory(name: String) = Threads.daemonFactory(name, ResourcePool.ThreadGroup)
}
