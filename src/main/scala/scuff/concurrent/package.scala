package scuff

import java.util.concurrent.{ Callable, Executor, ScheduledExecutorService }

import scala.concurrent.{ Await, ExecutionContext, Future, Promise, TimeoutException }
import scala.concurrent.duration.{ DurationInt, FiniteDuration }
import scala.util.{ Failure, Try }
import scala.util.control.NoStackTrace

import language.implicitConversions
import java.util.concurrent.ScheduledFuture

package object concurrent {
  implicit def exeCtxToExecutor(ec: ExecutionContext): Executor = ec match {
    case exe: Executor => exe
    case _ => new Executor with ExecutionContext {
      def execute(runnable: Runnable): Unit = ec.execute(runnable)
      def reportFailure(th: Throwable): Unit = ec.reportFailure(th)
    }
  }
  implicit class ScuffExecutor(private val ec: Executor) extends AnyVal {
    def execute(thunk: => Unit): Unit = ec execute new Runnable {
      def run = thunk
    }
    def submit(runnable: Runnable): Future[Unit] = {
      val promise = Promise[Unit]
      ec execute new Runnable {
        def run = promise complete Try(runnable.run)
        override def hashCode = runnable.hashCode
      }
      promise.future
    }
    def submit[T](callable: Callable[T]): Future[T] = {
      val promise = Promise[T]
      ec execute new Runnable {
        def run = promise complete Try(callable.call)
        override def hashCode = callable.hashCode
      }
      promise.future
    }
    def submit[T](thunk: => T): Future[T] = {
      val promise = Promise[T]
      ec execute new Runnable {
        def run = promise complete Try(thunk)
      }
      promise.future
    }
  }

  implicit class ScuffScheduledExecutor(private val scheduler: ScheduledExecutorService) extends AnyVal {
    def schedule[T](delay: FiniteDuration)(thunk: => T): ScheduledFuture[T] = {
      val c = new Callable[T] {
        def call = thunk
      }
      scheduler.schedule(c, delay.length, delay.unit)
    }
    def scheduleAtFixedRate(initDelay: FiniteDuration, period: FiniteDuration)(thunk: => Unit): ScheduledFuture[Unit] = {
      val r = new Runnable {
        def run = thunk
      }
      val initialDelay = period.unit.convert(initDelay.length, initDelay.unit)
      scheduler.scheduleAtFixedRate(r, initialDelay, period.length, period.unit).asInstanceOf[ScheduledFuture[Unit]]
    }
    def scheduleWithFixedDelay(initDelay: FiniteDuration, period: FiniteDuration)(thunk: => Unit): ScheduledFuture[Unit] = {
      val r = new Runnable {
        def run = thunk
      }
      val initialDelay = period.unit.convert(initDelay.length, initDelay.unit)
      scheduler.scheduleWithFixedDelay(r, initialDelay, period.length, period.unit).asInstanceOf[ScheduledFuture[Unit]]
    }
  }

  val DefaultTimeout = 30.seconds

  implicit class ScuffScalaFuture[T](private val f: Future[T]) extends AnyVal {
    def await: T = await(DefaultTimeout)
    def await(maxWait: FiniteDuration): T =
      if (f.isCompleted) {
        f.value.get.get
      } else {
        Await.result(f, maxWait)
      }
    def flatten[A](implicit mustBeFuture: Future[A] =:= T): Future[A] = f.asInstanceOf[Future[Future[A]]].flatMap(identity)(Threads.PiggyBack)
    def withTimeout(timeout: FiniteDuration)(implicit scheduler: ScheduledExecutorService = Threads.DefaultScheduler): Future[T] = {
        def fulfill(promise: Promise[T], value: Try[T]): Boolean = {
          try {
            promise.complete(value)
            true
          } catch {
            case _: IllegalStateException => false
          }
        }
      if (f.isCompleted) f
      else {
        val promise = Promise[T]
        val cmd = new Runnable {
          def run {
            fulfill(promise, Failure(new TimeoutException(s"Timed out after $timeout") with NoStackTrace))
          }
        }
        val timeoutFuture = scheduler.schedule(cmd, timeout.length, timeout.unit)
        f.onComplete {
          case result =>
            if (fulfill(promise, result)) {
              timeoutFuture.cancel(false)
            }
        }(Threads.PiggyBack)
        promise.future
      }
    }
  }

  implicit class ScuffJavaFuture[T](private val f: java.util.concurrent.Future[T]) extends AnyVal {
    def asScala(implicit conv: java.util.concurrent.Future[T] => Future[T] = Threads.javaFutureConverter): Future[T] = conv(f)
  }

  implicit class ScuffLock(private val lock: java.util.concurrent.locks.Lock) extends AnyVal {

    def tryFor[T](dur: FiniteDuration)(thunk: => T): Option[T] = {
      if (lock.tryLock(dur.length, dur.unit)) try {
        Some(thunk)
      } finally {
        lock.unlock()
      }
      else None
    }

    def apply[T](thunk: => T): T = {
      lock.lockInterruptibly()
      try {
        thunk
      } finally {
        lock.unlock()
      }
    }
    def uninterruptibly[T](thunk: => T) = {
      lock.lock()
      try {
        thunk
      } finally {
        lock.unlock()
      }
    }
  }

  implicit class ScuffCondition(private val cond: java.util.concurrent.locks.Condition) extends AnyVal {
    def await(condition: => Boolean): Unit = while (!condition) cond.await()
    def signalIf(condition: Boolean): Unit = if (condition) cond.signal()
    def signalAllIf(condition: Boolean): Unit = if (condition) cond.signalAll()
  }

  implicit class ScuffConcurrentMap[K, V](private val cmap: collection.concurrent.Map[K, V]) extends AnyVal {
    /** Update if present; return updated value. */
    def updateIfPresent(k: K)(update: V => V): Option[V] = {
      cmap.get(k) flatMap { oldvalue =>
        val newvalue = update(oldvalue)
        if (cmap.replace(k, oldvalue, newvalue)) Some(newvalue)
        else updateIfPresent(k)(update)
      }
    }
    /** Update or insert; return upserted value. */
    def upsert(k: K, putIfAbsent: V)(updateIfPresent: V => V): V = {
      cmap.putIfAbsent(k, putIfAbsent) match {
        case None => putIfAbsent
        case Some(present) =>
          val update = updateIfPresent(present)
          if (cmap.replace(k, present, update)) {
            update
          } else {
            upsert(k, putIfAbsent)(updateIfPresent)
          }
      }
    }
  }

}
