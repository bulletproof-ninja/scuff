package scuff

import java.util.concurrent._
import javax.servlet._

/**
 * Simple trait to enable heart beating as
 * a means to keep a connection alive.
 */
trait HeartbeatSupport {

  trait Pacemaker {
    @volatile private[this] var alive = true
    def jolt()
    def joltInterval(): (Int, TimeUnit) = (10, TimeUnit.SECONDS)
    def intervalJitter: Float = 0.2f
    final def stop() = alive = false
    final def isAlive = alive
  }

  protected def newScheduler = Executors.newScheduledThreadPool(1)
  private[this] val scheduler = newScheduler
  protected def shutdownHeartbeats(): Unit = scheduler.shutdownNow()
  protected def start(pm: Pacemaker) = schedule(pm)
  private def schedule(pm: Pacemaker) {
    import math._
    val (dur, unit) = pm.joltInterval
    val intervalMs = unit.toMillis(dur)
    val absJitter = intervalMs * pm.intervalJitter
    val minInterval = intervalMs - absJitter
    val jitterRange = absJitter * 2
    val delayMs = round(random * jitterRange + minInterval)
    val nextJolt = new Runnable {
      def run = if (pm.isAlive) {
        var scheduleAgain = true
        try {
          pm.jolt()
        } catch {
          case t: Throwable ⇒ scheduleAgain = false
        }
        if (scheduleAgain) try {
          schedule(pm)
        } catch {
          case e: RejectedExecutionException ⇒ // Ok
        }
      }
    }
    scheduler.schedule(nextJolt, delayMs, TimeUnit.MILLISECONDS)
  }

}