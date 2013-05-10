package scuff

import java.util.concurrent._

/**
 * Simple publish/subscribe mechanism, with optional exception handling.
 */
class PubSub[G, E <% G](
  exceptionHandler: (Throwable) ⇒ Unit = (t: Throwable) ⇒ {},
  executor: Option[Executor] = None)
    extends Channel {

  def this(executor: Executor) = this(executor = Some(executor))
  type F = G
  type L = E ⇒ Unit

  private object ThreadGroup extends ThreadGroup(ThreadFactory.SystemThreadGroup, this.getClass.getName) {
    override def uncaughtException(t: Thread, e: Throwable) = exceptionHandler(e)
  }
  private[this] val subscribers = new CopyOnWriteArrayList[FilteringSubscriber]
  private[this] val exec = executor.getOrElse {
    Executors newSingleThreadExecutor ThreadFactory(this.getClass.getName, ThreadGroup)
  }

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
    def handle(e: E) = if (filter(e)) {
        exec execute new Runnable {
        def run = try {
          sub(e)
        } catch {
          case e: Exception ⇒ exceptionHandler(e)
        }
      }
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
