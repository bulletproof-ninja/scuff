package scuff.eventual.ddd

import scuff.ddd._
import concurrent._

/**
 * Trait that stores snapshots in a a [[concurrent.Map]]. Should be
 * used in conjunction with a limiter, something to determine if a given
 * revision should be snapshotted, e.g. [[FixedIntervalSnapshots]] or [[LoadTimeSnapshots]].
 * NOTICE: Methods [[loadSnapshot]] and [[saveSnapshot]] are blocking calls,
 * with map access done on the calling thread. This is done to avoid comparatively high
 * latency on a potentially very cheap operation. If this is not desirable,
 * override either or both and flatMap that Future.
 */
trait MapSnapshots[ID, AR <: AggregateRoot, CAT] extends EventStoreRepository[ID, AR, CAT] {

  /** Concurrent map implementation for snapshots. */
  protected def snapshots: collection.concurrent.Map[AR#ID, (S, Int)]

  @annotation.tailrec
  private def trySave(id: AR#ID, value: (S, Int)) {
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
  protected override def saveSnapshot(id: AR#ID, revision: Int, state: S) = trySave(id, state -> revision)
  protected override def loadSnapshot(id: AR#ID) = Future.successful(snapshots.get(id))

}