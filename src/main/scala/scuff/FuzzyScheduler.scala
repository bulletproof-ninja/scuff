package scuff

import java.util.concurrent._
import javax.servlet._

object FuzzyScheduler {
  trait FuzzyRunnable extends Runnable {
    @volatile private[this] var alive = true
    def runDelay(): (Int, TimeUnit)
    def delayJitter: Float = 0.2f
    def kill() = alive = false
    final def isAlive = alive
  }
}

/**
  * Simple scheduler with built-in random jitter
  * to avoid pathological cases of clustering.
  */
class FuzzyScheduler(scheduler: ScheduledExecutorService = Executors.newScheduledThreadPool(1)) {
  import FuzzyScheduler.FuzzyRunnable

  def shutdownAll(): Unit = scheduler.shutdownNow()

  def schedule(pr: FuzzyRunnable) {
    import math._
    val (dur, unit) = pr.runDelay
    val intervalMs = unit.toMillis(dur)
    val absJitter = intervalMs * pr.delayJitter
    val minDelay = intervalMs - absJitter
    val jitterRange = absJitter * 2
    val delayMs = round(random * jitterRange + minDelay)
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