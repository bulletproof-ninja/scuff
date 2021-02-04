package scuff.concurrent

import java.util.concurrent._

class QueueFullRejection(r: Runnable, e: ThreadPoolExecutor, queueCapacity: Int)
extends RejectedExecutionException(s"Task $r rejected from $e because ${e.getQueue.getClass.getSimpleName}(capacity=$queueCapacity) is full")
