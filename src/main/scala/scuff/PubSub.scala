package scuff

import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

/**
 * Simple publish/subscribe mechanism.
 */
class PubSub[F, MSG <% F](notificationCtx: ExecutionContext)
    extends Faucet {

  type Filter = F
  type Consumer = MSG => Unit

  private[this] val subscribers = new java.util.concurrent.CopyOnWriteArrayList[FilteringSubscriber]

  /**
   * Publish message.
   */
  def publish(msg: MSG) {
    val i = subscribers.iterator
    while (i.hasNext) {
      i.next.handle(msg)
    }
  }

  private class FilteringSubscriber(sub: MSG => Unit, filter: Filter => Boolean) {
    def handle(msg: MSG) = try {
      if (filter(msg)) {
        notificationCtx execute new Runnable {
          def run = sub(msg)
        }
      }
    } catch {
      case NonFatal(e) =>
        subscribers.remove(this)
        notificationCtx.reportFailure(e)
    }
  }

  /**
   * Subscribe to events.
   */
  def subscribe(subscriber: MSG => Unit, filter: Filter => Boolean): Subscription = {
    val fs = new FilteringSubscriber(subscriber, filter)
    subscribers.add(fs)
    new Subscription {
      def cancel = subscribers.remove(fs)
    }
  }

}
