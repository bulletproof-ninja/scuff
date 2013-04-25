package scuff

object ThreadFactory {
  val SystemThreadGroup = rootThreadGroup(Thread.currentThread.getThreadGroup)
  private def rootThreadGroup(group: ThreadGroup): ThreadGroup = {
    if (group.getParent == null) group
    else rootThreadGroup(group.getParent)
  }
  private def newThreadGroup(name: String) = new ThreadGroup(ThreadFactory.SystemThreadGroup, name)

  def apply(name: String, threadGroup: ThreadGroup = null) = {
    val tg = if (threadGroup != null) threadGroup else newThreadGroup(name)
    new ThreadFactory(name, tg, tg, false)
  }
  def daemon(name: String, threadGroup: ThreadGroup = null) = {
    val tg = if (threadGroup != null) threadGroup else newThreadGroup(name)
    new ThreadFactory(name, tg, tg, true)
  }
}

class ThreadFactory(name: String, threadGroup: ThreadGroup, exceptionHandler: Thread.UncaughtExceptionHandler, daemon: Boolean) extends java.util.concurrent.ThreadFactory {
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