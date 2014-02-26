package scuff

import java.util.concurrent.ThreadFactory
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContextExecutor

/**
 * Thread helper class.
 */
object Threads {

  lazy val DefaultScheduler = Executors.newScheduledThreadPool(Runtime.getRuntime.availableProcessors, Threads.factory("scuff.DefaultScheduler"))

  /**
   * `ExecutionContext`, which executes on the same thread.
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
  private def newThreadGroup(name: String) = new ThreadGroup(Threads.SystemThreadGroup, name)

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
