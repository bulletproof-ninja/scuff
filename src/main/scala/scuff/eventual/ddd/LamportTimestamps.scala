package scuff.eventual.ddd

import scuff.ScuffScalaFuture
import concurrent.duration._
import scuff.Multiton
import scuff.LamportClock
import scuff.eventual.EventStore

trait LamportTimestamps { self: EventStoreRepository[_, _, _] =>

  private[this] val lamportClock = LamportTimestamps(eventStore)

  protected final def newTimestamp(causalTimestamp: Long): Long = lamportClock.next(causalTimestamp)

}

private object LamportTimestamps {
  private def newInstance(es: EventStore[_, _, _]): LamportClock = {
    val lastTimestamp = es.lastTimestamp.get(5.seconds)
    val clock = new LamportClock(lastTimestamp)
    es.subscribe(txn => clock.sync(txn.timestamp), _ => true)
    clock
  }
  private[this] val onePerEventStore = new Multiton(newInstance)
  def apply(es: EventStore[_, _, _]) = onePerEventStore(es)
}
