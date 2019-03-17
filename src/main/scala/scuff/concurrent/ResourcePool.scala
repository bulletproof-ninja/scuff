package scuff.concurrent

import java.util.concurrent._
import atomic.{ AtomicInteger, AtomicReference }

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.{ Duration, FiniteDuration }
import scala.reflect.{ ClassTag, classTag }
import scala.util.Try
import scala.util.control.NonFatal

import scuff.JMX

trait ResourcePool[R <: AnyRef] {

  protected def instanceName: String

  override def toString() = s"${getClass.getSimpleName}($instanceName = $availableCount)}"

  /**
   * Start a thread to evict resources that have
   * not been used for at least the given minimum
   * timeout.
   * @param minimumTimeout The minimum amount of time a resource has been unused before being eligible for eviction.
   * @param executor Scheduler or thread on which to run eviction.
   */
  def startEviction(
      minimumTimeout: FiniteDuration,
      executor: Executor = Threads.DefaultScheduler): ScheduledFuture[Nothing]

  /**
   *  Keep resources hot.
   *  @param excludeHottest Exclude if resource has been used within the given duration.
   *  Use zero to include all. Must be less than interval.
   *  @param interval The interval the heater is invoked
   *  @param executor The thread running the heater. If not a `ScheduledExecutorService`, a
   *  single thread will be monopolized entirely, until cancelled.
   */
  def startHeater(
      excludeHottest: FiniteDuration = Duration.Zero)(
      interval: FiniteDuration,
      executor: Executor = Threads.DefaultScheduler)(
      heater: R => Unit): ScheduledFuture[Nothing]

  def availableCount: Int

  /**
   * Drain pool of all resources.
   * NOTE: This ignores the `minResources`
   * setting.
   */
  def drain(): List[R]

  /** Use resource. */
  def use[A](thunk: R => A): A
}

abstract class BaseResourcePool[R <: AnyRef: ClassTag] protected (
    resourceFactory: => R,
    minResources: Int = 0,
    description: String)(
    implicit
    lifecycle: ResourcePool.Lifecycle[R])
  extends ResourcePool[R] {

  require(minResources >= 0, s"Cannot have less resources than zero: $minResources")

  private def newResource: R = try resourceFactory catch {
    case ex: ResourcePool.Exhausted => throw ex
    case NonFatal(cause) => throw new ResourcePool.ResourceUnavailable(cause)
  }

  protected def instanceName = {
    val className = classTag[R].runtimeClass.getName
    description match {
      case "" => className
      case desc => s"$className($desc)"
    }
  }

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
      def run(): Unit = {
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

  private final class Evictor(val timeout: FiniteDuration) extends Runnable {
    private[this] val timeoutMillis = timeout.toMillis
    require(timeoutMillis > 0, "Timeout must be > 0 milliseconds")

    def delay = new FiniteDuration(timeoutMillis * 2, TimeUnit.MILLISECONDS)
    def interval = new FiniteDuration((timeoutMillis / 4) max 1, TimeUnit.MILLISECONDS)

    def run = evictTail()

    @tailrec
    def evictTail(now: Long = currentMillis): Unit = {
      evictLast(now) match {
        case Some(evicted) =>
          Try(lifecycle.onEviction(evicted)) // Best effort cleanup
          evictTail(now)
        case _ => // Stop
      }
    }

    @tailrec
    def evictLast(now: Long): Option[R] = {
      pool.get match {
        case poolList if minResources == 0 || poolList.drop(minResources).nonEmpty =>
          poolList.reverse match {
            case (lastUsed, evicted) :: remaining if lastUsed + timeoutMillis < now =>
              if (pool.weakCompareAndSet(poolList, remaining.reverse)) {
                Some(evicted)
              } else {
                evictLast(now)
              }
            case _ => None
          }
        case _ => None
      }
    }
  }

  private final class Heater(heater: R => Unit, excludeHottestMillis: Long)
    extends Runnable {

    private def safeHeat(cool: R): Boolean = try { heater(cool); true } catch {
      case NonFatal(_) => false
    }

    @annotation.tailrec
    private def heatPool(): Unit = {
      val now = currentMillis
      val poolList = pool.get
        def isHot(t: (Long, R)): Boolean = now - t._1 < excludeHottestMillis
      val (hot, cool) = poolList.partition(isHot)
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

  def startEviction(
      minimumTimeout: FiniteDuration,
      executor: Executor = Threads.DefaultScheduler): ScheduledFuture[Nothing] = {

    if (evictor.get.isEmpty && evictor.compareAndSet(None, Some(new Evictor(minimumTimeout)))) {
      val newEvictor = evictor.get.get
      executor match {
        case scheduler: ScheduledExecutorService => schedule(scheduler, newEvictor, newEvictor.delay, newEvictor.interval)
        case _ => startThread(executor, newEvictor, newEvictor.delay, newEvictor.interval)
      }
    } else {
      throw new IllegalStateException("Eviction already started!")
    }
  }
  private[this] val evictor: AtomicReference[Option[Evictor]] = new AtomicReference(None)

  def startHeater(
      excludeHottest: FiniteDuration = Duration.Zero)(
      interval: FiniteDuration,
      executor: Executor = Threads.DefaultScheduler)(
      heater: R => Unit): ScheduledFuture[Nothing] = {

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

  def availableCount: Int = pool.get.size

  def drain(): List[R] = pool.getAndSet(Nil).map {
    case (_, res) =>
      Try(lifecycle.onEviction(res)) // Best effort cleanup
      res
  }

  /**
   * Pop (checkout) resource.
   * Can be overridden (as `public`) to
   * hold on to resource across threads.
   * @see push()
   */
  protected def pop(): R = popUntilSuccessful()

  /**
   * Push (return) resource.
   * If `pop()` is `public`, then
   * this should also be overridden.
   * @see pop()
   */
  protected def push(r: R): Unit = {
    val tuple = currentMillis -> lifecycle.onReturn(r)
    val list = pool.get
    if (!pool.weakCompareAndSet(list, tuple :: list)) {
      pushUntilSuccessful(List(tuple))
    }
  }

  @tailrec
  private def popUntilSuccessful(): R = {
    pool.get match {
      case Nil =>
        newResource match {
          case null => throw new IllegalStateException("Resource constructor returned `null`.")
          case r => lifecycle.onCheckout(r)
        }
      case list @ (_, head) :: tail =>
        if (pool.weakCompareAndSet(list, tail)) {
          lifecycle.onCheckout(head)
        } else {
          popUntilSuccessful()
        }
    }
  }

  @tailrec
  private def pushUntilSuccessful(append: List[(Long, R)]): Unit = {
    val list = pool.get
    val sorted = (list ++ append).sorted(ResourcePool.ordering)
    if (!pool.weakCompareAndSet(list, sorted)) {
      pushUntilSuccessful(append)
    }
  }

  def use[A](thunk: R => A): A = {
    val r = pop()
    val a = try thunk(r) catch {
      case NonFatal(cause) =>
        if (lifecycle evictOnFailure cause) {
          Try(lifecycle onEviction r)
        } else {
          push(r)
        }
        throw cause
    }
    push(r)
    a
  }

  protected def mxBean: ResourcePool.ResourcePoolMXBean = new ResourcePoolBean
  protected class ResourcePoolBean extends ResourcePool.ResourcePoolMXBean {
    def drain(): Unit = BaseResourcePool.this.drain()
    def getAvailableCount: Int = BaseResourcePool.this.availableCount
    def getActiveCount: Int = -1
    def getMinActive = minResources
    def getMaxActive = Int.MaxValue
    def getResourceTimeout: String = evictor.get.map(_.timeout.toString) getOrElse "<no timeout>"
    def startEviction(resourceTimeout: Int, resourceTimeoutUnit: String): Unit = {
      val minTimeout = FiniteDuration(resourceTimeout, resourceTimeoutUnit)
      BaseResourcePool.this.startEviction(minTimeout)
    }
  }

  JMX.register(mxBean, instanceName)
}

object ResourcePool {
  def apply[R <: AnyRef: ClassTag](
      newResource: R, minResources: Int = 0, maxResources: Int = Int.MaxValue,
      description: String = "")(
      implicit
      lifecycle: ResourcePool.Lifecycle[R] = ResourcePool.DefaultLifecycle[R]): ResourcePool[R] =
        if (maxResources == Int.MaxValue) {
          new UnboundedResourcePool(newResource, minResources, description)
        } else {
          new BoundedResourcePool(newResource, minResources, maxResources, description)
        }

  private[concurrent] val ordering = Ordering.by[(Long, _), Long](_._1).reverse

  trait Lifecycle[R] {
    /** Perform any necessary housekeeping before checking out. */
    def onCheckout(r: R): R
    /** Perform any necessary housekeeping after returned. */
    def onReturn(r: R): R
    /** Perform any necessary housekeeping when evicted, for any reason, including failure. */
    def onEviction(r: R): Unit
    /** Should resource be evicted on this failure? */
    def evictOnFailure(cause: Throwable): Boolean
  }
  /**
   *  Default `Lifecycle`.
   *  Will always defensively evict resource on failure and
   *  `close()` if possible.
   *  NOTE: Evicting on failure is often not what's desired,
   *  so generally use a custom lifecycle.
   */
  def DefaultLifecycle[R](): Lifecycle[R] = new Lifecycle[R] {
    def onCheckout(r: R): R = r
    def onReturn(r: R): R = r
    def onEviction(r: R): Unit = r match {
      case r: AutoCloseable => r.close()
      case _ => // noop
    }
    def evictOnFailure(cause: Throwable): Boolean = true
  }
  /**
   * Make lifecycle instance with partial evaluation of
   * which exceptions to not cause eviction.
   * NOTE return `false` to keep in pool, i.e. _not_ evict.
   */
  def onEviction[R](f: R => Unit)(evictOn: PartialFunction[Throwable, Boolean] = PartialFunction.empty): Lifecycle[R] =
    new Lifecycle[R] {
      def onCheckout(r: R): R = r
      def onReturn(r: R): R = r
      def onEviction(r: R): Unit = f(r)
      def evictOnFailure(cause: Throwable): Boolean = if (evictOn isDefinedAt cause) evictOn(cause) else true
    }
  def onCheckoutReturn[R](checkingOut: R => Unit, returning: R => Unit): Lifecycle[R] =
    new Lifecycle[R] {
      def onCheckout(r: R): R = { checkingOut(r); r }
      def onReturn(r: R): R = { returning(r); r }
      def onEviction(r: R): Unit = r match {
        case r: AutoCloseable => r.close()
        case _ => // noop
      }
      def evictOnFailure(cause: Throwable): Boolean = true
    }

  private[concurrent] trait ResourcePoolMXBean {
    def drain(): Unit
    def getAvailableCount: Int
    def getActiveCount: Int
    def getMinActive: Int
    def getMaxActive: Int
    def getResourceTimeout: String
    def startEviction(resourceTimeout: Int, resourceTimeoutUnit: String): Unit
  }

  class ResourceUnavailable protected (message: String, cause: Throwable = null)
    extends IllegalStateException(message, cause) {
    def this(cause: Throwable) = this(s"Cannot create new resource", cause)
  }

  final case class Exhausted(maxResources: Int, resourceType: Class[AnyRef])
    extends ResourcePool.ResourceUnavailable(
      s"No available ${resourceType.getName}. All $maxResources are currently in use.")

}

/**
 * Unbounded lock-free resource pool.
 *
 * Unlike traditional resource pools, the pool has
 * no upper limit on resources being created, so use
 * [[ResourcePool]]  if that is a concern.
 *
 * The intent is a potential substitute for [[ThreadLocal]]
 * but without necessarily creating the same resources number
 * of resources as threads. Resource can also be shared
 * across threads, through `pop()`/`push(resource)`, but
 * must be done with case to avoid escaping resources.
 *
 * Should really only be used for very short usage, but with
 * the option of getting a fresh resource on nested access.
 *
 * NOTICE: As with any pool, make absolutely sure the
 * resource does not escape the `use` scope, but
 * that almost goes without saying, amirite?
 */
class UnboundedResourcePool[R <: AnyRef: ClassTag](
    resourceFactory: => R,
    minResources: Int = 0,
    description: String = "")(
    implicit
    lifecycle: ResourcePool.Lifecycle[R] = ResourcePool.DefaultLifecycle[R])
  extends BaseResourcePool[R](resourceFactory, minResources, description) {

  override def pop() = super.pop()
  override def push(r: R) = super.push(r)

}

private object BoundedResourcePool {
  private def newTracker[R: ClassTag](resourceFactory: => R, min: Int, max: Int): Tracker[R] = {
    require(max >= min, s"Must have `maxResources` ($max) >= `minResources` ($min)")
    new Tracker(resourceFactory, max)
  }

  private final class Tracker[R: ClassTag](
      resourceFactory: => R, val max: Int) {
    require(max > 0, s"Must have `maxResources` ($max) > 0")

    private def resourceType = classTag[R].runtimeClass.asInstanceOf[Class[AnyRef]]
    private[this] val activeCounter = new AtomicInteger(0)
    def activeCount: Int = activeCounter.get
    def decrementActiveCount(by: Int = 1): Unit = {
      val active = activeCounter.addAndGet(-by)
      assert(active >= 0)
    }
    def newResource: R = {
        @tailrec
        def newResourceSafeIncrement(): R = {
          val currentActive = activeCounter.get
          if (currentActive == max) throw new ResourcePool.Exhausted(max, resourceType)
          else {
            assert(currentActive < max)
            if (activeCounter.compareAndSet(currentActive, currentActive + 1)) {
              try resourceFactory catch {
                case th: Throwable =>
                  decrementActiveCount()
                  throw th // Will be caught in `ResourcePool` instance and wrapped
              }
            } else newResourceSafeIncrement()
          }
        }
      newResourceSafeIncrement()
    }
  }
}

/**
 * Bounded resource pool.
 *
 * Non-blocking resource pool, with an upper limit to the
 * number of active resources.
 */
class BoundedResourcePool[R <: AnyRef: ClassTag] private (
    tracker: BoundedResourcePool.Tracker[R],
    minResources: Int,
    description: String)(
    implicit
    lifecycle: ResourcePool.Lifecycle[R])
  extends BaseResourcePool[R](tracker.newResource, minResources, description) {

  def this(
      newResource: R, minResources: Int, maxResources: Int,
      description: String = "")(
      implicit
      lifecycle: ResourcePool.Lifecycle[R]) =
    this(
      BoundedResourcePool.newTracker(newResource, minResources, maxResources),
      minResources, description)

  assert(tracker.activeCount == minResources)

  def activeCount = tracker.activeCount

  // public push/pop makes resource tracking impossible
  override final protected def pop() = super.pop()
  override final protected def push(r: R) = super.push(r)

  override def drain(): List[R] = {
    val drained = super.drain()
    tracker.decrementActiveCount(drained.size)
    drained
  }

  override def use[A](thunk: R => A): A =
    super.use { resource =>
      try thunk(resource) catch {
        case th: Throwable =>
          tracker.decrementActiveCount()
          throw th
      }
    }

  override protected def mxBean = new this.ResourcePoolBean {
    override def getActiveCount: Int = tracker.activeCount
    override def getMinActive = minResources
    override def getMaxActive = tracker.max
  }

  override def toString() = s"${getClass.getSimpleName}($instanceName = $activeCount)}"

}
