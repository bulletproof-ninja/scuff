package scuff.eventual

import scuff._
import concurrent.{ Promise, Future }
import java.util.Date

/**
 * Event source.
 */
trait EventSource[ID, EVT, CAT] extends Channel {
  final type F = CAT
  type L = Transaction ⇒ Unit

  case class Transaction(
    timestamp: Long,
    category: CAT,
    streamId: ID,
    revision: Int,
    metadata: Map[String, String],
    events: List[EVT]) extends {
    private def writeObject(out: java.io.ObjectOutputStream) {
      out.writeLong(this.timestamp)
      out.writeObject(this.category)
      out.writeObject(this.streamId)
      out.writeInt(this.revision)
      if (this.metadata.isEmpty) {
        out.writeBoolean(false)
      } else {
        out.writeBoolean(true)
        out.writeObject(this.metadata)
      }
      out.writeObject(this.events)

    }
    private def readObject(in: java.io.ObjectInputStream) {
      val surgeon = new Surgeon(this)
      surgeon.set('timestamp, in.readLong)
      surgeon.set('category, in.readObject)
      surgeon.set('streamId, in.readObject)
      surgeon.set('revision, in.readInt)
      val metadata = if (in.readBoolean) in.readObject else Map.empty
      surgeon.set('metadata, metadata)
      surgeon.set('events, in.readObject)
    }
  }

  def exists(stream: ID): Future[Boolean]

  def replayStream[T](stream: ID)(callback: Iterator[Transaction] ⇒ T): Future[T]
  def replayStreamSince[T](stream: ID, sinceRevision: Int)(callback: Iterator[Transaction] ⇒ T): Future[T]
  def replayStreamTo[T](stream: ID, toRevision: Int)(callback: Iterator[Transaction] ⇒ T): Future[T]
  def replayStreamRange[T](stream: ID, revisionRange: collection.immutable.Range)(callback: Iterator[Transaction] ⇒ T): Future[T]

  /**
   * Play back transactions, optionally filtered by one or more categories.
   * This is a blocking call, i.e. when call returns, playback has finished.
   * @param categories: Optional categories filter
   * @param callback Callback function
   */
  def replay[T](categories: CAT*)(callback: Iterator[Transaction] ⇒ T): Future[T]

  /**
   * Play back events for all instances from a given time forward, optionally
   * filtered by one or more categories.
   * This is a blocking call, i.e. when call returns, playback has finished.
   * @param fromTime Only play back transactions since the provided timestamp.
   * @param categories: Optional categories filter
   * @param callback Callback function
   */
  def replayFrom[T](fromTime: Date, categories: CAT*)(callback: Iterator[Transaction] ⇒ T): Future[T]

}

/**
 * Event store.
 */
trait EventStore[ID, EVT, CAT] extends EventSource[ID, EVT, CAT] {

  /**
   * Record events into a particular stream, then publish the transaction to subscribers.
   * @param streamId Event stream identifier
   * @param revision Event stream revision, which is expected to be committed
   * @param events The events
   * @return Potential DuplicateRevisionException if the expected revision has already been committed. Try, try again.
   */
  def record(category: CAT, streamId: ID, revision: Int, events: List[_ <: EVT], metadata: Map[String, String] = Map.empty): Future[Transaction]

  /**
   * Append events into a particular stream, then publish the transaction to subscribers.
   * @param category The category for this stream
   * @param streamID Event stream identifier
   * @param events The events
   * @param metadata Optional metadata
   * @return Event stream transaction that was committed
   */
  def append(category: CAT, streamID: ID, events: List[_ <: EVT], metadata: Map[String, String] = Map.empty): Future[Transaction]
}
