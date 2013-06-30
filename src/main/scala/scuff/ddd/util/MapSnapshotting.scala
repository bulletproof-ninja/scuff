package scuff.ddd.util

import scuff.ddd._
import concurrent._

/**
 * Trait that enables snapshotting at particular
 * intervals, using a [[concurrent.Map]].
 * NOTICE: Methods [[loadSnapshot]] and [[saveSnapshot]] are blocking calls,
 * with map access done on the calling thread. If this is not desirable,
 * override either or both and flatMap that Future.
 */
trait MapSnapshotting[ID, AR <: AggregateRoot, CAT] extends EventStoreRepository[ID, AR, CAT] {

  /** Concurrent map implementation for snapshots. */
  protected def snapshots: collection.concurrent.Map[AR#ID, (S, Long)]
  /** Interval between snapshots. */
  protected def saveInterval: Int

  @annotation.tailrec
  private def trySave(id: AR#ID, value: (S, Long)) {
    snapshots.putIfAbsent(id, value) match {
      case None ⇒ // Success
      case Some(other) ⇒
        if (value._2 > other._2) { // replace with later revision
          if (!snapshots.replace(id, other, value)) {
            trySave(id, value)
          }
        }
    }
  }
  protected override def saveSnapshot(id: AR#ID, revision: Long, state: S) = {
    if ((revision + 1) % saveInterval == 0) {
      trySave(id, state -> revision)
    }
  }
  protected override def loadSnapshot(id: AR#ID) = Future.successful(snapshots.get(id))

}