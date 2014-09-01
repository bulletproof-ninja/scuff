package scuff.eventual.ddd.cqrs

trait StreamTracker {
  type ID
  /** Last timestamp seen. */
  def lastTimestamp: Option[Long]
  /** Expected stream revision. */
  def expectedRevision(id: ID): Int
  /** Mark id/revision as consumed. */
  def markAsConsumed(id: ID, rev: Int, timestamp: Long): Unit
  /** Callback when generator goes live. */
  def onGoingLive(): Unit

}
