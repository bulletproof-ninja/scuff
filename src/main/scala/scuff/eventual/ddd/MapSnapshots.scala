package scuff.eventual.ddd

import scuff.ddd._
import concurrent._

/**
 * Trait that stores snapshots in a a [[concurrent.Map]]. Should probably be
 * used in conjunction with a limiter, something to determine if a given
 * revision should be snapshotted, e.g. [[FixedIntervalSnapshots]] or [[LoadTimeSnapshots]],
 * unless memory consumption is a non-issue.
 */
trait MapSnapshots[ID, AR <: AggregateRoot, CAT] extends EventStoreRepository[ID, AR, CAT] {

  /** Concurrent map implementation for snapshots. */
  protected def snapshots: collection.concurrent.Map[AR#ID, Snapshot]

  @annotation.tailrec
  private def trySave(id: AR#ID, snapshot: Snapshot) {
    snapshots.putIfAbsent(id, snapshot) match {
      case None => // Success
      case Some(other) =>
        if (snapshot.revision > other.revision) { // replace with later revision
          if (!snapshots.replace(id, other, snapshot)) {
            trySave(id, snapshot)
          }
        }
    }
  }
  protected override def saveSnapshot(id: AR#ID, snapshot: Snapshot) = trySave(id, snapshot)
  protected override def loadSnapshot(id: AR#ID) = Future successful snapshots.get(id)

}
