package scuff

import java.util.concurrent._
import javax.servlet._

object FuzzyScheduler {
  trait FuzzyRunnable extends Runnable {
    @volatile private[this] var alive = true
    /**
      * Handle exception.
      * Rethrow to bubble up to underlying executor service
      * (NOTE that this will potentially affect other jobs),
      * or call `stop()` to stop orderly. Or do something
      * else and continue scheduling.
      * This method exists to ensure conscious exception
      * handling. It could be done inside `run` method,
      * but it's often forgotten.
      */
    def onException(th: Throwable)
    /**
      * Delay between executions. On initial invocation,
      * this is the delay after scheduling, on subsequent
      * invocations, it's the delay after the previous run
      * ended.
      */
    def runDelay(): (Int, TimeUnit)
    /**
      * The jitter margin applied to the `runDelay`.
      */
    def delayJitter: Float = 0.2f

    /**
      * Stop further scheduling.
      */
    def stop() = alive = false
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
        try {
          pr.run()
        } catch {
          case th: Throwable ⇒ pr.onException(th)
        }
        if (pr.isAlive) try {
          schedule(pr)
        } catch {
          case e: RejectedExecutionException ⇒ // Ok
        }
      }
    }
    scheduler.schedule(nextRun, delayMs, TimeUnit.MILLISECONDS)
  }

}