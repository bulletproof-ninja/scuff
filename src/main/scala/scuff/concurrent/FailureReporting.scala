package scuff.concurrent

import java.util.concurrent.ThreadPoolExecutor
import scala.concurrent.ExecutionContextExecutor
import java.util.concurrent.ExecutionException
import java.lang.reflect.InvocationTargetException

trait FailureReporting
  extends ThreadPoolExecutor
  with ExecutionContextExecutor {

  final override protected def afterExecute(r: Runnable, t: Throwable): Unit = {
    super.afterExecute(r, t)
    if (t != null) {
      reportFailure(t)
    }
  }

  def reportFailure(th: Throwable) = th match {
    case e: ExecutionException => reportFailure(e.getCause)
    case e: InvocationTargetException => reportFailure(e.getCause)
    case _ => Thread.currentThread.getUncaughtExceptionHandler match {
      case null => th.printStackTrace(System.err)
      case ueh => ueh.uncaughtException(Thread.currentThread, th)
    }
  }
}
