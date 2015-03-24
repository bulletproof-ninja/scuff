package scuff

import scala.concurrent._
import java.util.concurrent.Callable
import scala.util.Try
import scala.concurrent.duration.Duration
import scuff.concurrent.Threads
import java.util.concurrent.Executor
import language.implicitConversions

package object concurrent {
  implicit def exeCtxToExecutor(ec: ExecutionContext): Executor = new Executor {
    def execute(runnable: Runnable): Unit = ec.execute(runnable)
  }
  implicit final class ScuffExecutor(val ec: Executor) extends AnyVal {
    def execute(thunk: => Unit): Unit = ec execute new Runnable {
      def run = thunk
    }
    def submit(runnable: Runnable): Future[Unit] = {
      val promise = Promise[Unit]
      ec execute new Runnable {
        def run = promise complete Try(runnable.run)
      }
      promise.future
    }
    def submit[T](callable: Callable[T]): Future[T] = {
      val promise = Promise[T]
      ec execute new Runnable {
        def run = promise complete Try(callable.call())
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

  implicit final class ScuffScalaFuture[T](val f: Future[T]) extends AnyVal {
    def get(implicit maxWait: Duration): T =
      if (f.isCompleted) {
        f.value.get.get
      } else {
        Await.result(f, maxWait)
      }
  }

  implicit final class ScuffJavaFuture[T](val f: java.util.concurrent.Future[T]) extends AnyVal {
    def asScala(implicit conv: java.util.concurrent.Future[T] => Future[T] = Threads.javaFutureConverter): Future[T] = conv(f)
  }

  implicit final class ScuffLock(val lock: java.util.concurrent.locks.Lock) extends AnyVal {
    def whenLocked[T](code: => T): T = {
      lock.lock()
      try {
        code
      } finally {
        lock.unlock()
      }
    }
  }

}
