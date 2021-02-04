package scuff.concurrent

import java.util.concurrent._

class ExecutorShutdownRejection(r: Runnable, e: ThreadPoolExecutor)
extends RejectedExecutionException(s"Task $r rejected from $e because it has been shut down.")
