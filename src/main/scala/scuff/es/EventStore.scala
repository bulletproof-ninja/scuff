package scuff.es

private object EventSource {
  final def writeTransaction(t: AnyRef, out: java.io.ObjectOutputStream) {
    val txn = t.asInstanceOf[EventSource[Any, Any]#Transaction]
    val transBytes = txn.transactionID.toByteArray
    out.writeByte(transBytes.length)
    out.write(transBytes)
    out.writeLong(txn.timestamp.asMillis)
    out.writeObject(txn.streamID)
    out.writeLong(txn.revision)
    out.writeObject(txn.events)
  }

  final def readTransaction(txn: AnyRef, in: java.io.ObjectInputStream) {
    val surgeon = new scuff.Surgeon(txn)
    val transBytes = new Array[Byte](scuff.BitsBytes.unsigned(in.readByte))
    in.readFully(transBytes)
    surgeon.setField('transactionID, BigInt(transBytes))
    surgeon.setField('timestamp, new scuff.Timestamp(in.readLong))
    surgeon.setField('streamID, in.readObject)
    surgeon.setField('revision, in.readLong)
    surgeon.setField('events, in.readObject)
  }

}

/**
 * Event source.
 */
trait EventSource[ID, EVT] extends scuff.Topic {
  final type T = Transaction

  case class Transaction(
    transactionID: BigInt,
    timestamp: scuff.Timestamp,
    streamID: ID,
    revision: Long,
    events: List[_ <: EVT]) extends {
    private def writeObject(out: java.io.ObjectOutputStream) = EventSource.writeTransaction(this, out)
    private def readObject(in: java.io.ObjectInputStream) = EventSource.readTransaction(this, in)
  }

  def replayStream[T](stream: ID)(callback: Iterator[Transaction] ⇒ T): T
  def replayStreamSince[T](stream: ID, sinceRevision: Long)(callback: Iterator[Transaction] ⇒ T): T
  def replayStreamTo[T](stream: ID, toRevision: Long)(callback: Iterator[Transaction] ⇒ T): T
  def replayStreamRange[T](stream: ID, revisionRange: collection.immutable.NumericRange[Long])(callback: Iterator[Transaction] ⇒ T): T

  /**
   * Play back transactions, optionally only the most recent.
   * This is a blocking call, i.e. when call returns, playback has finished.
   * @param sinceTransactionID Optional. Only play back transactions since the provided transactionID (not included in playback). Defaults to -1 (all).
   * @param callback Callback function
   */
  def replay[T](sinceTransactionID: BigInt = BigInt(-1))(callback: Iterator[Transaction] ⇒ T): T

  /**
   * Play back events for all instances from a given time forward.
   * This is a blocking call, i.e. when call returns, playback has finished.
   * @param fromTime Only play back transactions since the provided timestamp.
   * @param callback Callback function
   */
  def replaySince[T](fromTime: java.util.Date)(callback: Iterator[Transaction] ⇒ T): T

}

/**
 * Event store.
 */
trait EventStore[ID, EVT] extends EventSource[ID, EVT] {

  /**
   * Record events into a particular stream, then publish the transaction to subscribers.
   * @param streamID Event stream identifier
   * @param revision Event stream revision, which is expected to be committed
   * @param events The events
   * @throws DuplicateRevisionException if the expected revision has already been committed. Try, try again.
   */
  @throws(classOf[DuplicateRevisionException])
  def record(streamID: ID, revision: Long, events: List[_ <: EVT])

  /**
   * Record events into a particular stream, then publish the transaction to subscribers.
   * @param streamID Event stream identifier
   * @param events The events
   * @return revision Event stream revision, which was committed
   */
  def record(streamID: ID, events: List[_ <: EVT]): Long
}

