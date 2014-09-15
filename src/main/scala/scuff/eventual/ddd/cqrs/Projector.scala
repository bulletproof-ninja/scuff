package scuff.eventual.ddd.cqrs

import scuff.Subscription
import scuff.ddd.AggregateRoot
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import scuff.Faucet
import scala.util.Try
import concurrent.duration._

trait Projector {

  /** Public publish format. */
  type PUB
  /** Internal data format. */
  protected type DAT <: Data
  /** Internal filter. */
  protected type F <: Filter

  protected trait Data {
    def toPublish(to: F): PUB
  }

  protected trait Filter extends (DAT => Boolean) {
    @inline
    final def apply(data: DAT): Boolean = matches(data)
    def matches(data: DAT): Boolean
  }

  protected val store: DataStore

  protected def query(filter: F)(receiver: DAT => Unit)(implicit conn: store.R): Unit

  protected def faucet: Faucet {
    type L = (DAT => Unit)
    type F = DAT
  }

  /**
   * @param filter The subscription/query filter
   * @param subscriber The callback function
   * @param strict If `true`, eliminates the, perhaps remote, possibility of out-of-order revisions
   */
  protected final def subscribe(filter: F, subscriber: PUB => Unit, strict: Boolean = true): Subscription = {
    val latch = newLatch(strict)
      def proxySubscriber(data: DAT) {
        latch.await()
        subscriber(data.toPublish(filter))
      }
    subscribeToFaucet(filter, subscriber, proxySubscriber, latch)
  }

  private def subscribeToFaucet(filter: F, realSubscriber: PUB ⇒ Unit, proxySubscriber: DAT ⇒ Unit, latch: Latch): Subscription = {
    val subscription = faucet.subscribe(proxySubscriber, filter)
    try {
      store.readOnly { implicit conn =>
        query(filter) { data =>
          val msg = data.toPublish(filter)
          realSubscriber(msg)
        }
      }
    } catch {
      case t: Throwable ⇒
        Try(subscription.cancel)
        throw t
    } finally {
      latch.open()
    }
    subscription
  }

  private trait Latch {
    @inline
    def await()
    def open()
  }

  /** When using `strict` subscribe, max time to wait for the initial data lookup. */
  protected val maxDataStoreLookupWaitOnStrict: FiniteDuration = 5.seconds

  private def newLatch(strict: Boolean): Latch = {
    if (strict) {
      new Latch {
        val cdl = new CountDownLatch(1)
        @inline
        def await = cdl.await(maxDataStoreLookupWaitOnStrict.length, maxDataStoreLookupWaitOnStrict.unit)
        def open = cdl.countDown()
      }
    } else {
      new Latch {
        @inline
        def await = ()
        def open = ()
      }
    }
  }

}
