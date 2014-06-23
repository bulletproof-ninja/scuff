package scuff

import java.util.concurrent.ThreadFactory
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContextExecutor
import java.util.concurrent.Executor
import scala.concurrent.Promise
import scala.util.Try

/**
 * Thread helper class.
 */
object Threads {

  private[this] lazy val _javaFutureConverter = {
    val jfg = new JavaFutureConverter[Any]
    jfg.thread.start()
    jfg
  }
  def javaFutureConverter[T] = _javaFutureConverter.asInstanceOf[JavaFutureConverter[T]]
  class JavaFutureConverter[T](sleepMs: Long = 1, failureReporter: Throwable => Unit = (t) => t.printStackTrace)
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

  lazy val DefaultScheduler = Executors.newScheduledThreadPool(Runtime.getRuntime.availableProcessors, Threads.daemonFactory("scuff.DefaultScheduler"))

  def newSingleRunExecutor(tf: ThreadFactory): Executor = new Executor {
    def execute(r: Runnable) {
      val thread = tf.newThread(r)
      r.run()
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

  object Blocking extends ExecutionContextExecutor {
    private[this] val pool = Executors.newCachedThreadPool(daemonFactory(getClass.getName, newThreadGroup(getClass.getName)))
    def execute(r: Runnable) = pool.execute(r)
    def reportFailure(t: Throwable) = t.printStackTrace()
  }

  val PiggyBack = new SameThreadExecutor {
    def reportFailure(t: Throwable) = throw t
  }

  val SystemThreadGroup = rootThreadGroup(Thread.currentThread.getThreadGroup)
  private def rootThreadGroup(group: ThreadGroup): ThreadGroup = {
    if (group.getParent == null) group
    else rootThreadGroup(group.getParent)
  }
  private def newThreadGroup(
    name: String,
    failureReporter: Throwable => Unit = (t: Throwable) => t.printStackTrace) =
    new ThreadGroup(Threads.SystemThreadGroup, name) {
      override def uncaughtException(t: Thread, e: Throwable) {
        failureReporter(e)
      }
    }

  def factory(name: String, threadGroup: ThreadGroup = null): ThreadFactory = {
    val tg = if (threadGroup != null) threadGroup else newThreadGroup(name)
    new ScuffThreadFactory(name, tg, tg, false)
  }
  def daemonFactory(name: String, threadGroup: ThreadGroup = null): ThreadFactory = {
    val tg = if (threadGroup != null) threadGroup else newThreadGroup(name)
    new ScuffThreadFactory(name, tg, tg, true)
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
