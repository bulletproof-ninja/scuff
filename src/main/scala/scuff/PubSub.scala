package scuff

import java.util.concurrent._

/**
 * Simple publish/subscribe mechanism, with optional exception handling.
 */
class PubSub[E](exceptionHandler: (Throwable) ⇒ Unit = (t: Throwable) ⇒ {}, executor: Option[Executor] = None)
    extends Channel {

  type L = E ⇒ Unit

  def this(executor: Executor) = this(executor = Some(executor))

  private object ThreadGroup extends ThreadGroup(ThreadFactory.SystemThreadGroup, this.getClass.getName) {
    override def uncaughtException(t: Thread, e: Throwable) = exceptionHandler(e)
  }
  private[this] val subscribers = new CopyOnWriteArraySet[E ⇒ Unit]
  private[this] val exec = executor.getOrElse {
    Executors newSingleThreadExecutor ThreadFactory(this.getClass.getName, ThreadGroup)
  }

  /**
   * Publish event.
   */
  def publish(e: E) {
    val i = subscribers.iterator
    while (i.hasNext) {
      val subscriber = i.next
      try {
        exec execute new Runnable {
          def run = subscriber(e)
        }
      } catch {
        case t: Exception ⇒ exceptionHandler(t)
      }
    }
  }

  /**
   * Subscribe to events.
   * NOTICE: This method is idempotent, so adding the same
   * subscriber more than once has no effect and will
   * not lead to multiple notifications.
   */
  def subscribe(subscriber: E ⇒ Unit): Subscription = {
    subscribers.add(subscriber)
    new Subscription {
      def cancel = subscribers.remove(subscriber)
    }
  }

}
