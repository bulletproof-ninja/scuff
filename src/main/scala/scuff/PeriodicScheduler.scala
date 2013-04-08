package scuff

import java.util.concurrent._
import javax.servlet._

object PeriodicScheduler {
  trait PeriodicRunnable extends Runnable {
    @volatile private[this] var alive = true
    def runInterval(): (Int, TimeUnit) = (10, TimeUnit.SECONDS)
    def intervalJitter: Float = 0.2f
    def kill() = alive = false
    final def isAlive = alive
  }
}

/**
  * Simple scheduler with built-in random jitter
  * to avoid pathological cases of clustering.
  */
class PeriodicScheduler(scheduler: ScheduledExecutorService = Executors.newScheduledThreadPool(1)) {
  import PeriodicScheduler.PeriodicRunnable

  def shutdownAll(): Unit = scheduler.shutdownNow()

  def schedule(pr: PeriodicRunnable) {
    import math._
    val (dur, unit) = pr.runInterval
    val intervalMs = unit.toMillis(dur)
    val absJitter = intervalMs * pr.intervalJitter
    val minInterval = intervalMs - absJitter
    val jitterRange = absJitter * 2
    val delayMs = round(random * jitterRange + minInterval)
    val nextRun = new Runnable {
      def run = if (pr.isAlive) {
        var scheduleAgain = true
        try {
          pr.run()
        } catch {
          case e: Exception ⇒
            e.printStackTrace()
            scheduleAgain = false
        }
        if (scheduleAgain && pr.isAlive) try {
          schedule(pr)
        } catch {
          case e: RejectedExecutionException ⇒ // Ok
        }
      }
    }
    scheduler.schedule(nextRun, delayMs, TimeUnit.MILLISECONDS)
  }

}