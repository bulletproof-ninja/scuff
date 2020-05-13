package scuff.concurrent

import scala.concurrent.Future
import java.util.concurrent.Semaphore
import scala.util.control.NonFatal

final class BlockingBoundedFuturesIterator[T](
  window: Int,
  iter: Iterator[T],
  future: T => Future[_])
  extends Iterator[T] {

  require(window > 0, s"Must have an active window size of at least 1, was $window")

  private[this] val sem = new Semaphore(window)

  def hasNext: Boolean = iter.hasNext

  def next(): T = {
    sem.acquire()
    val elem = try iter.next catch {
      case NonFatal(cause) =>
        sem.release()
        throw cause
    }
    future(elem) match {
      case future if future.isCompleted => sem.release()
      case active => active.onComplete { _ => sem.release() }(Threads.PiggyBack)
    }
    elem
  }

}
