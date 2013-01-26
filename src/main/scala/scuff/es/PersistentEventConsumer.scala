package scuff.es

/**
 * Event stream consumer. Must be fully thread-safe
 * as no guarantee can be made about the threading
 * model.
 */
trait PersistentEventConsumer[ID, EVT] {

  type TXN = EventSource[ID, EVT]#Transaction

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
  def lastProcessedRev(id: ID): Option[Long]
  /**
    * Last processed transaction.
    * This is used when resuming and thus
    * will typically only be called during startup.
    */
  def lastProcessedTxn(): Option[BigInt]
}
