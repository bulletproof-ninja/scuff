package scuff.concurrent

import scala.concurrent._, duration._
import scala.util.control.NonFatal
import scala.util.Try

object JavaFutureConverter {
  def apply(
      failureReporter: Throwable => Unit, 
      sleep: FiniteDuration = 1.millisecond): JavaFutureConverter = {
    val conv = new JavaFutureConverter(failureReporter, sleep)
    conv.thread.start()
    conv
  }
}

final class JavaFutureConverter private (failureReporter: Throwable => Unit, sleep: FiniteDuration)
  extends (java.util.concurrent.Future[Any] => Future[Any]) {

  private type QueueItem = (Promise[Any], java.util.concurrent.Future[Any])
  private[this] val queue = new collection.mutable.Queue[QueueItem]
  private val thread = new Thread(classOf[JavaFutureConverter].getName) {
    this setUncaughtExceptionHandler new Thread.UncaughtExceptionHandler {
      def uncaughtException(t: Thread, e: Throwable): Unit = {
        failureReporter(e)
      }
    }
    override def run(): Unit = {
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

  def apply(f: java.util.concurrent.Future[Any]): Future[Any] = {
    if (f.isDone) {
      try Future successful f.get catch { case NonFatal(e) => Future failed e }
    } else {
      val promise = Promise[Any]
      queue.synchronized {
        val notifyOnContent = queue.isEmpty
        queue enqueue promise -> f
        if (notifyOnContent) queue.notify()
      }
      promise.future
    }
  }
}
