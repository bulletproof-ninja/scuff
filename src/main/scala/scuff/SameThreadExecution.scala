package scuff

/**
 * `ExecutionContext`, which executes on the same thread.
 */
abstract class SameThreadExecution extends concurrent.ExecutionContextExecutor {
  def execute(runnable: Runnable) = try { runnable.run() } catch { case t: Throwable ⇒ reportFailure(t) }
}

object SameThreadExecution extends SameThreadExecution {
  def reportFailure(t: Throwable) = t.printStackTrace()
}
