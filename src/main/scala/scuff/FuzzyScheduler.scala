package scuff

import java.util.concurrent._
import scala.concurrent.duration._
import javax.servlet._
import scala.concurrent.ExecutionContext

object FuzzyScheduler {
  trait FuzzyRunnable extends Runnable {
    @volatile private[this] var alive = true
    /**
      * Handle exception.
      * Rethrow to bubble up to underlying executor service
      * (NOTE: rethrow will potentially affect other jobs),
      * or call `stop()` to stop this orderly.
      * Or do something else and continue scheduling.
      * This method exists to ensure excplicit exception
      * handling. It could be done inside `run` method,
      * but it's often forgotten.
      */
    def onException(th: Throwable)
    /**
      * Interval between executions. On initial invocation,
      * this is the delay after scheduling, on subsequent
      * invocations, it's the delay after the previous run
      * ended.
      */
    def runInterval(): Duration
    /**
      * The jitter margin applied to the `runInterval`.
      */
    def intervalJitter: Float = 0.2f

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
class FuzzyScheduler(scheduler: ScheduledExecutorService = Executors.newScheduledThreadPool(1, ThreadFactory(classOf[FuzzyScheduler].getName))) {
  import FuzzyScheduler.FuzzyRunnable

  def shutdownAll(): Unit = scheduler.shutdownNow()

  val executionContext = ExecutionContext.fromExecutorService(scheduler)

  def schedule(pr: FuzzyRunnable) {
    import math._
    val intervalMs = pr.runInterval.toMillis
    val absJitter = intervalMs * pr.intervalJitter
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
