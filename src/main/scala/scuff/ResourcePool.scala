package scuff

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.util.Try
import scala.concurrent.duration.Duration

/**
 * Unbounded lock-free resource pool.
 * Any resource is deliberately discarded when
 * an exception occurs, to avoid potentially
 * corrupted resources. This behavior can easily
 * be changed on a case-by-case basis by subclassing
 * and overriding `borrow` and preventing any exceptions
 * in this class.
 * This pool can be used as a more efficient replacement
 * for [[ThreadLocal]], in that it will never create more
 * instances than there are threads, like [[ThreadLocal]],
 * but has much higher probability for creating less.
 * NOTICE: As with any pool, make absolutely sure the
 * resource does not escape the `borrow` scope.
 */
class ResourcePool[R](constructor: ⇒ R) {

  /**
   * Start a thread to prune resources that have
   * not been used for at least the given minimum
   * timeout.
   */
  def startPruningThread(minimumResourceTimeout: Duration, destructor: R ⇒ Unit = _ ⇒ Unit) {
    require(minimumResourceTimeout.isFinite, "Timeout must be a finite duration")
    val timeoutMillis = minimumResourceTimeout.toMillis
    require(timeoutMillis > 0, "Timeout must be > 0 milliseconds")
    val thread = Threads.daemonFactory(getClass.getName, ResourcePool.ThreadGroup) newThread new Runnable {
      @tailrec
      def prune(list: List[(Long, R)], now: Long = System.currentTimeMillis, pruned: List[R] = Nil): (List[(Long, R)], List[R]) = {
        list match {
          case (lastUsed, r) :: tail if lastUsed + timeoutMillis < now ⇒
            prune(tail, now, r :: pruned)
          case _ ⇒
            list -> pruned
        }
      }
      def run {
        Thread.sleep(timeoutMillis * 2) // Initial sleep
        val sleepyTime = (timeoutMillis / 4).max(1)
        while (!Thread.currentThread.isInterrupted) {
          val poolList = pool.get
          val (remaining, pruned) = prune(poolList.reverse)
          if (pruned.nonEmpty) {
            if (pool.compareAndSet(poolList, remaining.reverse)) {
              pruned.foreach(r ⇒ Try(destructor(r)))
            }
          }
          Thread.sleep(sleepyTime)
        }
      }
    }
    thread.start()
  }

  def size = pool.get.size

  private val pool = new AtomicReference[List[(Long, R)]](Nil)
  @tailrec
  private def pop(): R = {
    pool.get match {
      case Nil ⇒
        constructor match {
          case null ⇒ throw new IllegalStateException("Resource constructor returned `null`.")
          case r ⇒ r
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
   * NOTICE: This will not be called on
   * a newly constructed resource, only
   * one already in the pool.
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
}
