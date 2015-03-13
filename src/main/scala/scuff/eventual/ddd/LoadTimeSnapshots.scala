package scuff.eventual.ddd

import scuff.ddd._

/**
 * Trait that enables snapshotting if load time
 * exceeds a certain threshold.
 */
trait LoadTimeSnapshots[ID, AR <: AggregateRoot, CAT] extends EventStoreRepository[ID, AR, CAT] {

  /**
   * Load threshold (in ms) to trigger snapshot.
   */
  protected def loadThreshold: Long

  private[this] val doSnapshot = new scuff.LockFreeConcurrentMap[AR#ID, Int]

  protected abstract override def onLoadNotification(id: AR#ID, revision: Int, category: CAT, timeMs: Long) {
    if (timeMs >= loadThreshold) {
      doSnapshot.putIfAbsent(id, revision)
    }
    super.onLoadNotification(id, revision, category, timeMs)
  }

  protected abstract override def saveSnapshot(id: AR#ID, snapshot: Snapshot) {
    if (doSnapshot.remove(id, snapshot.revision)) {
      super.saveSnapshot(id, snapshot)
    }
  }

}
