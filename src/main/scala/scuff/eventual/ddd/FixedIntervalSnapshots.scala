package scuff.eventual.ddd

import scuff.ddd._

/**
 * Trait that enables snapshotting at particular
 * revision intervals, using a [[concurrent.Map]].
 */
trait FixedIntervalSnapshots[ID, AR <: AggregateRoot, CAT] extends EventStoreRepository[ID, AR, CAT] {

  /**
   * Interval between revisions.
   */
  protected def revisionInterval: Int

  protected override def assumeCurrentSnapshot = revisionInterval == 1

  protected abstract override def saveSnapshot(id: AR#ID, revision: Long, state: S) {
    if ((revision + 1) % revisionInterval == 0) {
      super.saveSnapshot(id, revision, state)
    }
  }

}
