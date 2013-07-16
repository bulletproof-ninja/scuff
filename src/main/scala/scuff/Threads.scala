package scuff

import java.util.concurrent.ThreadFactory

object Threads {
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