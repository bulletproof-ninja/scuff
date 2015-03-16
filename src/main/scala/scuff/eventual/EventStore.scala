package scuff.eventual

import java.io.InvalidObjectException

import scala.concurrent.Future
import scala.util.control.NoStackTrace

import scuff.Faucet

/**
 * Event source.
 */
trait EventSource[ID, EVT, CAT] extends Faucet {
  final type F = CAT
  type L = Transaction => Unit

  case class Transaction(
      timestamp: Long,
      category: CAT,
      streamId: ID,
      revision: Int,
      metadata: Map[String, String],
      events: List[EVT]) {
    private def writeObject(out: java.io.ObjectOutputStream) {
      out.writeLong(this.timestamp)
      out.writeObject(this.category)
      out.writeObject(this.streamId)
      out.writeInt(this.revision)
      out.writeChar(this.metadata.size)
      this.metadata.foreach {
        case (key, value) =>
          out.writeUTF(key)
          out.writeUTF(value)
      }
      this.events.foreach(out.writeObject)
      out.writeObject(null)
    }
    private def readEvents(in: java.io.ObjectInputStream): List[EVT] = in.readObject match {
      case null => Nil
      case evt => evt.asInstanceOf[EVT] :: readEvents(in)
    }
    private def readObject(in: java.io.ObjectInputStream) {
      val surgeon = new scuff.reflect.Surgeon(this)
      surgeon.set('timestamp, in.readLong)
      surgeon.set('category, in.readObject)
      surgeon.set('streamId, in.readObject)
      surgeon.set('revision, in.readInt)
      val mapSize: Int = in.readChar
      var metadata = Map.empty[String, String]
      while (metadata.size < mapSize) {
        metadata += in.readUTF -> in.readUTF
      }
      surgeon.set('metadata, metadata)
      surgeon.set('events, readEvents(in))
    }

    private def readObjectNoData(): Unit = throw new InvalidObjectException("Stream data required")
  }

  def exists(stream: ID): Future[Boolean]

  /**
   * Last timestamp recorded.
   * If no events have yet been recorded, return a timestamp
   * that is smaller than (or equal) to any possible
   * timestamp (typically `0`, `-1`, or `Long.MinValue`).
   */
  def lastTimestamp: Future[Long]

  def replayStream[T](stream: ID)(callback: Iterator[Transaction] => T): Future[T]
  def replayStreamAfter[T](stream: ID, afterRevision: Int)(callback: Iterator[Transaction] => T): Future[T]
  def replayStreamTo[T](stream: ID, toRevision: Int)(callback: Iterator[Transaction] => T): Future[T]
  def replayStreamRange[T](stream: ID, revisionRange: collection.immutable.Range)(callback: Iterator[Transaction] => T): Future[T]

  /**
   * Play back transactions, optionally filtered by one or more categories.
   * @param categories: Optional categories filter
   * @param callback Callback function
   */
  def replay[T](categories: CAT*)(callback: Iterator[Transaction] => T): Future[T]

  /**
   * Play back events for all instances from a given time forward, optionally
   * filtered by one or more categories.
   * This is a blocking call, i.e. when call returns, playback has finished.
   * @param fromTimestamp Only play back transactions since the provided timestamp (inclusive).
   * @param categories: Optional categories filter
   * @param callback Callback function
   */
  def replayFrom[T](fromTimestamp: Long, categories: CAT*)(callback: Iterator[Transaction] => T): Future[T]

}

/**
 * Event store.
 */
trait EventStore[ID, EVT, CAT] extends EventSource[ID, EVT, CAT] {

  class DuplicateRevisionException(id: Any, val conflictingTransaction: this.Transaction)
    extends RuntimeException(s"Revision ${conflictingTransaction.revision} already exists: $id")
    with NoStackTrace

  /**
   * Record events into a particular stream, then publish the transaction to subscribers.
   * @param streamId Event stream identifier
   * @param revision Event stream revision, which is expected to be committed
   * @param events The events
   * @return Potential DuplicateRevisionException if the expected revision has already been committed. Try, try again.
   */
  def record(timestamp: Long, category: CAT, streamId: ID, revision: Int, events: List[_ <: EVT], metadata: Map[String, String] = Map.empty): Future[Transaction]

  //  /**
  //   * Append events into a particular stream, then publish the transaction to subscribers.
  //   * @param category The category for this stream
  //   * @param streamID Event stream identifier
  //   * @param events The events
  //   * @param metadata Optional metadata
  //   * @return Event stream transaction that was committed
  //   */
  //  def append(category: CAT, streamID: ID, events: List[_ <: EVT], metadata: Map[String, String] = Map.empty): Future[Transaction]
}
