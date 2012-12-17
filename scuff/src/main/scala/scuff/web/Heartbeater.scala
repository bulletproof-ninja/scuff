package scuff.web

import java.util.concurrent._
import javax.servlet._

trait Heartbeater extends Servlet {
  /**
    * Current connections as keep-alive functions.
    */
  protected def connections(): Iterator[() ⇒ Unit]

  protected def interval(): (Int, TimeUnit) = (10, TimeUnit.SECONDS)
  protected def newScheduler = Executors.newSingleThreadScheduledExecutor()
  protected val scheduler = newScheduler

  abstract override def init(config: ServletConfig) {
    super.init(config)
    val (time, unit) = interval
    val heartbeater = new Runnable {
      def run {
        val iter = connections()
        while (iter.hasNext) try {
          iter.next.apply()
        } catch {
          case _ ⇒ // Ignore
        }
      }
    }
    scheduler.scheduleAtFixedRate(heartbeater, time, time, unit)
  }

}