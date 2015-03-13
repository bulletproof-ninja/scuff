package scuff.eventual.ddd

trait WallClockTimestamps { self: EventStoreRepository[_, _, _] =>

  protected final def newTimestamp(causalTimestamp: Long): Long = (causalTimestamp + 1) max System.currentTimeMillis

}
