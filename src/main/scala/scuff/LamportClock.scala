package scuff

import java.util.concurrent.atomic.AtomicLong
import scala.annotation.tailrec

final class LamportClock(init: Long = 0) {

  private[this] val counter = new AtomicLong(init)

  def next(): Long = counter.incrementAndGet()

  @tailrec
  def next(sync: Long): Long = {
    val last = counter.get
    val nextTs = (last max sync) + 1
    if (counter.compareAndSet(last, nextTs)) {
      nextTs
    } else {
      next(sync)
    }
  }

  @tailrec
  def sync(update: Long): Unit = {
    val curr = counter.get
    if (update > curr && !counter.compareAndSet(curr, update)) {
      sync(update)
    }
  }

}
