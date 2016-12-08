package scuff

import scala.concurrent._
import java.util.concurrent.Callable
import scala.util.Try
import scala.concurrent.duration.Duration
import scuff.concurrent.Threads
import java.util.concurrent.Executor
import language.implicitConversions
import scala.concurrent.duration._
import java.util.concurrent.ScheduledExecutorService
import scala.util.control.NoStackTrace
import scala.util.Failure

package object concurrent {
  implicit def exeCtxToExecutor(ec: ExecutionContext): Executor = ec match {
    case exe: Executor => exe
    case _ => new Executor {
      def execute(runnable: Runnable): Unit = ec.execute(runnable)
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

}
