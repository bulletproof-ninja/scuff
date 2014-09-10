package scuff

import java.util.concurrent.ThreadFactory
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContextExecutor
import java.util.concurrent.Executor
import scala.concurrent.Promise
import scala.util.Try
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.ExecutorService
import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.SynchronousQueue
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.ScheduledThreadPoolExecutor

/**
 * Thread helper class.
 */
object Threads {

  val SystemThreadGroup = rootThreadGroup(Thread.currentThread.getThreadGroup)

  val PiggyBack = new SameThreadExecutor {
    def reportFailure(t: Throwable) = throw t
  }

  private[this] val ScuffThreadGroup = newThreadGroup(getClass.getName, daemon = false, printStackTrace)
  final val Blocking = {
    val exec = newCachedThreadPool(factory(getClass.getName, ScuffThreadGroup), t => () /* Will be reported from ExecutionContext */)
    ExecutionContext.fromExecutor(exec, printStackTrace)
  }

  private def printStackTrace(t: Throwable): Unit = 
    t.printStackTrace(System.err)

  private[this] lazy val _javaFutureConverter = {
    val jfg = new JavaFutureConverter[Any]
    jfg.thread.start()
    jfg
  }
  def javaFutureConverter[T] = _javaFutureConverter.asInstanceOf[JavaFutureConverter[T]]
  class JavaFutureConverter[T](sleepMs: Long = 1, failureReporter: Throwable => Unit = printStackTrace)
      extends (java.util.concurrent.Future[T] => concurrent.Future[T]) {
    type QueueItem[T] = (Promise[T], java.util.concurrent.Future[T])
    private[this] val queue = new collection.mutable.Queue[QueueItem[T]]
    private[Threads] val thread = new Thread("scuff.Threads.JavaFutureGetter") {
      this setUncaughtExceptionHandler new Thread.UncaughtExceptionHandler {
        def uncaughtException(t: Thread, e: Throwable) {
          failureReporter(e)
        }
      }
      override def run {
        while (!Thread.currentThread.isInterrupted) {
          queue.synchronized {
            if (queue.isEmpty) {
              queue.wait()
            } else {
              queue.dequeueAll(_._2.isDone()).foreach {
                case (promise, f) => promise complete Try(f.get)
              }
              Thread sleep sleepMs
            }
          }
        }
      }
    }
    def apply(f: java.util.concurrent.Future[T]): concurrent.Future[T] = {
      val promise = Promise[T]
      queue.synchronized {
        val notify = queue.isEmpty
        queue enqueue promise -> f
        queue.notify()
      }
      promise.future
    }
  }

  def newScheduledThreadPool(corePoolSize: Int, threadFactory: ThreadFactory, failureReporter: Throwable => Unit = printStackTrace) = {
    val exec = new ScheduledThreadPoolExecutor(corePoolSize, threadFactory) {
      override def afterExecute(r: Runnable, t: Throwable) {
        super.afterExecute(r, t)
        if (t != null) {
          failureReporter(t)
        }
      }
    }
    Runtime.getRuntime addShutdownHook new Thread {
      override def run {
        exec.shutdownNow()
      }
    }
    exec
  }

  lazy val DefaultScheduler = newScheduledThreadPool(Runtime.getRuntime.availableProcessors, Threads.factory("scuff.DefaultScheduler", ScuffThreadGroup))

  /** Executor encapsulating a single `Thread` for a single execution. Cannot be re-used. */
  def newSingleRunExecutor(tf: ThreadFactory, failureReporter: Throwable => Unit = printStackTrace): Executor = new Executor {
    def execute(r: Runnable) {
      val thread = tf newThread new Runnable {
        def run = try {
          r.run()
        } catch {
          case t: Throwable => failureReporter(t)
        }
      }
      thread.start()
    }
  }

  /**
   * `ExecutionContext`, which executes on the same thread.
   * Use this to prevent a cheap execution having to pass through
   * another to another thread, and only use this when you're certain
   * that it's OK to execute on the existing thread.
   */
  abstract class SameThreadExecutor extends concurrent.ExecutionContextExecutor {
    def execute(runnable: Runnable) = runnable.run()
  }

  def newCachedThreadPool(threadFactory: ThreadFactory, failureReporter: Throwable => Unit = printStackTrace): ExecutorService = {
    val exec = new ThreadPoolExecutor(0, Integer.MAX_VALUE,
      60L, TimeUnit.SECONDS,
      new SynchronousQueue[Runnable],
      threadFactory) {
      override def afterExecute(r: Runnable, t: Throwable) {
        super.afterExecute(r, t)
        if (t != null) {
          failureReporter(t)
        }
      }
    }
    Runtime.getRuntime addShutdownHook new Thread {
      override def run {
        exec.shutdownNow()
      }
    }
    exec
  }

  def newSingleThreadExecutor(threadFactory: ThreadFactory, failureReporter: Throwable => Unit = printStackTrace) = {
    val exec = new ThreadPoolExecutor(1, 1,
      0L, TimeUnit.MILLISECONDS,
      new LinkedBlockingQueue[Runnable],
      threadFactory) {
      override def afterExecute(r: Runnable, t: Throwable) {
        super.afterExecute(r, t)
        if (t != null) {
          failureReporter(t)
        }
      }
    }
    Runtime.getRuntime addShutdownHook new Thread {
      override def run {
        exec.shutdownNow()
      }
    }
    exec
  }

  private def rootThreadGroup(group: ThreadGroup): ThreadGroup = {
    if (group.getParent == null) group
    else rootThreadGroup(group.getParent)
  }
  def newThreadGroup(
    name: String,
    daemon: Boolean,
    failureReporter: Throwable => Unit = printStackTrace) = {
    val tg = new ThreadGroup(Threads.SystemThreadGroup, name) {
      override def uncaughtException(t: Thread, e: Throwable) {
        failureReporter(e)
      }
    }
    tg.setDaemon(daemon)
    tg
  }

  def factory(name: String, threadGroup: ThreadGroup = null): ThreadFactory = {
    val tg = if (threadGroup != null) threadGroup else newThreadGroup(name, daemon = false)
    new ScuffThreadFactory(name, tg, tg, false)
  }
  def daemonFactory(name: String, threadGroup: ThreadGroup = null): ThreadFactory = {
    val tg = if (threadGroup != null) threadGroup else newThreadGroup(name, daemon = true)
    new ScuffThreadFactory(name, tg, tg, true)
  }

  final class ExecutorProxy[E <: Executor](real: E, reporter: Throwable => Unit) extends Executor {
    implicit def executor = real
    def execute(cmd: Runnable) {
      real execute new Runnable {
        def run = try {
          cmd.run()
        } catch {
          case t: Throwable => reporter(t); throw t
        }
      }
    }
  }

}

private[scuff] class ScuffThreadFactory(name: String, threadGroup: ThreadGroup, exceptionHandler: Thread.UncaughtExceptionHandler, daemon: Boolean) extends ThreadFactory {
  private val nameFormat = name + "[%d]"
  private val counter = new java.util.concurrent.atomic.AtomicInteger
  private def newName = nameFormat.format(counter.getAndIncrement)
  def newThread(runnable: Runnable) = {
    val t = new Thread(threadGroup, runnable, newName)
    t.setUncaughtExceptionHandler(exceptionHandler)
    t.setDaemon(daemon)
    t
  }
}
