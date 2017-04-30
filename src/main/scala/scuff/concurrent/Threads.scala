package scuff.concurrent

import java.util.concurrent.{ Executor, LinkedBlockingQueue, ScheduledExecutorService, ScheduledThreadPoolExecutor, SynchronousQueue, ThreadFactory, ThreadPoolExecutor, TimeUnit }
import scala.concurrent.{ ExecutionContext, ExecutionContextExecutor, ExecutionContextExecutorService, Future, Promise }
import scala.util.Try
import scala.util.control.NonFatal
import scala.concurrent.duration._

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
    val exec = newCachedThreadPool(factory(s"${getClass.getName}.Blocking", ScuffThreadGroup))
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
  class JavaFutureConverter[T](sleep: FiniteDuration = 1.millisecond, failureReporter: Throwable => Unit = printStackTrace)
      extends (java.util.concurrent.Future[T] => Future[T]) {
    private type QueueItem = (Promise[T], java.util.concurrent.Future[T])
    private[this] val queue = new collection.mutable.Queue[QueueItem]
    private[Threads] val thread = new Thread("scuff.Threads.JavaFutureConverter") {
      this setUncaughtExceptionHandler new Thread.UncaughtExceptionHandler {
        def uncaughtException(t: Thread, e: Throwable) {
          failureReporter(e)
        }
      }
      override def run {
        while (!Thread.currentThread.isInterrupted) {
          val completed = queue.synchronized {
            while (queue.isEmpty) {
              queue.wait()
            }
            queue.dequeueAll(_._2.isDone())
          }
          if (completed.nonEmpty) {
            completed.foreach {
              case (promise, f) => promise complete Try(f.get)
            }
          } else {
            sleep.unit.sleep(sleep.length)
          }
        }
      }
    }
    def apply(f: java.util.concurrent.Future[T]): Future[T] = {
      if (f.isDone) {
        try Future successful f.get catch { case NonFatal(e) => Future failed e }
      } else {
        val promise = Promise[T]
        queue.synchronized {
          val notifyOnContent = queue.isEmpty
          queue enqueue promise -> f
          if (notifyOnContent) queue.notify()
        }
        promise.future
      }
    }
  }

  def newScheduledThreadPool(
    corePoolSize: Int, threadFactory: ThreadFactory,
    failureReporter: Throwable => Unit = printStackTrace): ScheduledExecutorService with ExecutionContext = {

    val exec = new ScheduledThreadPoolExecutor(corePoolSize, threadFactory) with ExecutionContext {
      override def afterExecute(r: Runnable, t: Throwable) {
        super.afterExecute(r, t)
        if (t != null) {
          reportFailure(t)
        }
      }
      def reportFailure(t: Throwable) = failureReporter(t)
    }
    Runtime.getRuntime addShutdownHook new Thread {
      override def run {
        exec.shutdownNow()
      }
    }
    exec
  }

  def newBlockingThread[T](name: String)(blocking: => T): Future[T] = {
    val done = Promise[T]
    val t = new Thread(name) {
      override def run() = {
        done complete Try(blocking)
      }
    }
    t.start()
    done.future
  }

  lazy val DefaultScheduler = newScheduledThreadPool(Runtime.getRuntime.availableProcessors, Threads.factory("scuff.DefaultScheduler", ScuffThreadGroup))

  /** Executor encapsulating a single `Thread` for a single execution. Cannot be re-used. */
  def newSingleRunExecutor(tf: ThreadFactory, failureReporter: Throwable => Unit = printStackTrace): ExecutionContextExecutor = new ExecutionContextExecutor {
    def execute(r: Runnable) {
      val thread = tf newThread new Runnable {
        def run = try {
          r.run()
        } catch {
          case t: Throwable => reportFailure(t)
        }
      }
      thread.start()
    }
    def reportFailure(t: Throwable) = failureReporter(t)
  }

  /**
    * `ExecutionContext`, which executes on the same thread.
    * Use this to prevent a cheap execution having to pass through
    * another to another thread, and only use this when you're certain
    * that it's OK to execute on the existing thread.
    */
  abstract class SameThreadExecutor extends ExecutionContextExecutor {
    def execute(runnable: Runnable) = runnable.run()
  }

  def newCachedThreadPool(threadFactory: ThreadFactory, failureReporter: Throwable => Unit = printStackTrace): ExecutionContextExecutorService = {
    val exec = new ThreadPoolExecutor(1, Short.MaxValue,
      60L, TimeUnit.SECONDS,
      new SynchronousQueue[Runnable],
      threadFactory) with ExecutionContextExecutorService {

      override def afterExecute(r: Runnable, t: Throwable) {
        super.afterExecute(r, t)
        if (t != null) {
          reportFailure(t)
        }
      }
      def reportFailure(t: Throwable) = failureReporter(t)
    }
    Runtime.getRuntime addShutdownHook new Thread {
      override def run {
        exec.shutdownNow()
      }
    }
    exec
  }

  def newSingleThreadExecutor(threadFactory: ThreadFactory, failureReporter: Throwable => Unit = printStackTrace): ExecutionContextExecutorService = {
    val exec = new ThreadPoolExecutor(1, 1,
      0L, TimeUnit.MILLISECONDS,
      new LinkedBlockingQueue[Runnable],
      threadFactory) with ExecutionContextExecutorService {

      override def afterExecute(r: Runnable, t: Throwable) {
        super.afterExecute(r, t)
        if (t != null) {
          reportFailure(t)
        }
      }
      def reportFailure(t: Throwable) = failureReporter(t)
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
  def newThreadFactory(threadGroup: ThreadGroup): ThreadFactory = {
    new ScuffThreadFactory(threadGroup.getName, threadGroup, threadGroup, threadGroup.isDaemon)
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
  private val counter = new java.util.concurrent.atomic.AtomicInteger
  private def newName = s"$name.${counter.getAndIncrement}"
  def newThread(runnable: Runnable) = {
    val t = new Thread(threadGroup, runnable, newName)
    t.setUncaughtExceptionHandler(exceptionHandler)
    t.setDaemon(daemon)
    t
  }
}
