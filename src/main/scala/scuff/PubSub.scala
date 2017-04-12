package scuff

import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

/**
  * Simple publish/subscribe mechanism.
  * @param consumerCtx The execution context that consumers will
  * be notified on. Consider using a
  * [[scuff.concurrent.PartitionedExecutionContext]] if
  * same-thread notification is important.
  */
class PubSub[F, MSG <% F](consumerCtx: ExecutionContext) extends Feed {

  type Selector = F
  type Consumer = MSG => Unit

  private[this] val subscribers = new java.util.concurrent.CopyOnWriteArrayList[FilteringSubscriber]

  protected def newRunnable(hash: Int)(r: => Unit): Runnable = new Runnable {
    def run = r
    override def hashCode = hash
  }

  /**
    * Publish message.
    */
  def publish(msg: MSG) {
    val i = subscribers.iterator
    while (i.hasNext) {
      i.next.handle(msg)
    }
  }

  private class FilteringSubscriber(
      consumer: Consumer, include: F => Boolean, hash: Int) {
    def handle(msg: MSG) = try {
      if (include(msg)) {
        consumerCtx execute newRunnable(hash) {
          try consumer.apply(msg) catch {
            case NonFatal(e) =>
              consumerCtx reportFailure e
              cancelSubscription()
          }
        }
      }
    } catch {
      case NonFatal(e) =>
        consumerCtx reportFailure e
        cancelSubscription()
    }
    def cancelSubscription() {
      subscribers.remove(this)
    }
  }

  /**
    * Subscribe to events.
    */
  def subscribe(
    filter: F => Boolean)(subscriber: Consumer): Subscription = {
    val fs = new FilteringSubscriber(subscriber, filter, subscriber.hashCode)
    subscribers.add(fs)
    new Subscription {
      def cancel = fs.cancelSubscription()
    }
  }

}
