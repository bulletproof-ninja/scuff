package scuff.es

/**
  * Event stream consumer. Must be fully thread-safe
  * as no guarantee can be made about the threading
  * model.
  */
trait PersistentEventConsumer[ID, EVT, CAT] {

  type TXN = EventSource[ID, EVT, CAT]#Transaction

  /**
    * Consume transaction.
    */
  def consume(txn: TXN)

  /**
    * Last processed revision for a given id.
    * This is used to detect gaps and duplicates
    * of individual streams and is called on every
    * transaction received.
    */
  def lastProcessedRev(streamId: ID): Option[Long]

  /**
    * Last processed transaction time.
    * This is used when resuming and thus
    * will typically only be called during startup.
    * It would be wise to adjust the time based on
    * the clock skew that might exist in a
    * sharded/clustered environment.
    */
  def resumeFrom(): Option[scuff.Timestamp]
}
