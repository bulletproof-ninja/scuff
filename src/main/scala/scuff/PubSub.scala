package scuff

import concurrent.ExecutionContext

/**
 * Simple publish/subscribe mechanism.
 */
class PubSub[G, E <% G](executor: ExecutionContext)
    extends Faucet {

  type F = G
  type L = E ⇒ Unit

  private[this] val subscribers = new java.util.concurrent.CopyOnWriteArrayList[FilteringSubscriber]

  /**
   * Publish event.
   */
  def publish(e: E) {
    val i = subscribers.iterator
    while (i.hasNext) {
      i.next.handle(e)
    }
  }

  private class FilteringSubscriber(sub: E ⇒ Unit, filter: F ⇒ Boolean) {
    def handle(e: E) = try {
      if (filter(e)) {
        executor execute new Runnable {
          def run = sub(e)
        }
      }
    } catch {
      case t: Throwable ⇒
        subscribers.remove(this)
        executor.reportFailure(t)
    }
  }

  /**
   * Subscribe to events.
   */
  def subscribe(subscriber: E ⇒ Unit, filter: F ⇒ Boolean): Subscription = {
    val fs = new FilteringSubscriber(subscriber, filter)
    subscribers.add(fs)
    new Subscription {
      def cancel = subscribers.remove(fs)
    }
  }

}
