package scuff.concurrent

import scuff._
import scala.concurrent._
import scala.util.Random

class RandomDelayExecutionContext(exeCtx: ExecutionContext) extends ExecutionContext {

  def execute(runnable: Runnable) = exeCtx execute new Runnable {
    def run(): Unit = {
      if (Random.nextBoolean) {
        val delay = Random.nextInRange(1 to 50)
        Thread sleep delay
      }
      runnable.run()
    }
  }

  def reportFailure(th: Throwable) = exeCtx reportFailure th

}

object RandomDelayExecutionContext
  extends RandomDelayExecutionContext(ExecutionContext.global)
