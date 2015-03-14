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

  protected type ES = EventStream[tracker.ID, DomainEvent, CAT]
  protected type TXN = ES#Transaction

  protected def consume(txn: ES#Transaction)(implicit conn: store.RW): Iterable[DAT]
  protected def publish(msgs: Iterable[DAT])(implicit conn: store.RW)

  /**
   * Resume event stream processing.
   * @param eventStream The event stream to consume
   * @param restart Restart processing from scratch.
   * NOTICE: This assumes that the data store has also been wiped appropriately
   */
  def resume(eventStream: ES): Future[Subscription] = {
    require(categoryFilter.nonEmpty, s"${getClass.getName}: Category filter cannot be empty")
    val liveSubscription = eventStream resume new eventStream.DurableConsumer {
      val categoryFilter = Generator.this.categoryFilter
      def lastTimestamp() = tracker.lastTimestamp
      def consumeReplay(txn: TXN) {
        val expected = tracker.expectedRevision(txn.streamId)
        if (txn.revision == expected) {
          store.readWrite(consume(txn)(_))
          tracker.markAsConsumed(txn.streamId, txn.revision, txn.timestamp)
        } else if (txn.revision > expected) {
          throw new IllegalStateException(s"${txn.category} ${txn.streamId} revision(s) missing. Got ${txn.revision}, but was epxecting $expected. This is either revisions missing from the EventSource or a bug in the EventStream implementation.")
        }
      }
      def onLive() = {
        tracker.onGoingLive()
        new LiveConsumer {
          def expectedRevision(streamId: tracker.ID): Int = tracker.expectedRevision(streamId)
          def consumeLive(txn: TXN) = store.readWrite { implicit conn =>
            val forPublish = consume(txn)
            tracker.markAsConsumed(txn.streamId, txn.revision, txn.timestamp)
            publish(forPublish)
          }
        }
      }

    }
    liveSubscription
  }

}
