package scuff.eventual.ddd

import scuff.ddd._

/**
 * Trait that enables snapshotting at particular
 * revision intervals. Does not implement snapshot
 * storage.
 */
trait FixedIntervalSnapshots[ID, AR <: AggregateRoot, CAT] extends EventStoreRepository[ID, AR, CAT] {

  /**
   * Interval between revisions.
   */
  protected def revisionInterval: Int

  protected override def assumeSnapshotCurrent = revisionInterval <= 2

  protected abstract override def saveSnapshot(id: AR#ID, snapshot: Snapshot) {
    if ((snapshot.revision + 1) % revisionInterval == 0) {
      super.saveSnapshot(id, snapshot)
    }
  }

}
