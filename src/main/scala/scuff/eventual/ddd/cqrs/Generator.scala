package scuff.eventual.ddd.cqrs

import scuff.eventual.EventStream
import scala.concurrent.Future
import scala.concurrent.duration._
import scuff.Subscription
import scuff.Timestamp
import scuff.ddd._
import language.implicitConversions

trait Generator extends Projector {

  /** Category filter type. */
  type CAT

  protected def categoryFilter: Set[CAT]
  
  protected val tracker: StreamTracker
  type RAWID = tracker.ID
  implicit protected def toRawID(aggrId: AID): RAWID

  /** Potential time skew on transactions. */
  protected val timestampSkew: FiniteDuration = 3.seconds

  protected type ES = EventStream[RAWID, DomainEvent, CAT]
  protected type TXN = ES#Transaction

  protected def consume(txn: ES#Transaction)(implicit conn: store.CONN): Iterable[DAT]
  protected def publish(msgs: Iterable[DAT])(implicit conn: store.CONN)

  /**
   * Resume event stream processing.
   * @param eventStream The event stream to consume
   * @param restart Restart processing from scratch. 
   * NOTICE: This assumes that the data store has also been wiped appropriately
   */
  def resume(eventStream: ES, restart: Boolean = false): Future[Subscription] = {
    require(categoryFilter.nonEmpty, "${getClass.getName}: Category filter cannot be empty")
    if (restart) {
      tracker.wipeAll()
    }
    eventStream resume new eventStream.DurableConsumer {
      val categoryFilter = Generator.this.categoryFilter
      def resumeFrom() = tracker.lastTimestamp.map(last => new Timestamp(last - timestampSkew.toMillis))
      def consumeHistoric(txn: TXN) {
        val expected = tracker.expectedRevision(txn.streamId)
        if (txn.revision == expected) {
          store.connect(consume(txn)(_))
          tracker.markAsConsumed(txn.streamId, txn.revision, txn.timestamp, commit = false)
        } else if (txn.revision > expected) {
          throw new IllegalStateException(s"${txn.category} ${txn.streamId} revision(s) missing. Got ${txn.revision}, but was epxecting $expected. This is most likely a concurrency problem.")
        }
      }
      def onLive() = {
        tracker.commit()
        new LiveConsumer {
          def nextExpectedRevision(streamId: tracker.ID): Int = tracker.expectedRevision(streamId)
          def consumeLive(txn: TXN) = store.connect { implicit conn ⇒
            val forPublish = consume(txn)
            tracker.markAsConsumed(txn.streamId, txn.revision, txn.timestamp, commit = true)
            publish(forPublish)
          }
        }
      }

    }
  }

}