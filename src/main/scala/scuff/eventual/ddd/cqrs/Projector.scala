package scuff.eventual.ddd.cqrs

import scuff.Subscription
import scuff.ddd.AggregateRoot
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import scuff.Faucet
import scala.util.Try
import concurrent.duration._

trait Projector {
  /** Aggregate ID type. */
  type AID
  /** Entiity ID type. */
  type EID
  /** Subscription identifier. */
  //  final type ID = (AID, Option[EID])
  /** Public publish format. */
  type PUB
  /** Internal data format. */
  protected type DAT <: Data
  /** Internal subscription id. */
  protected type SID <: SubscriptionID

  protected trait Data {
    def toPublish(subscriptionId: SID): PUB
  }

  protected def newSubscriptionID(aid: AID, eid: Option[EID]): SID
  protected trait SubscriptionID extends (DAT => Boolean) {
    @inline
    final def apply(data: DAT): Boolean = matches(data)
    def matches(data: DAT): Boolean
  }

  protected val store: DataStore

  protected def lookup(aid: AID, eid: Option[EID] = None)(implicit conn: store.CONN): Option[DAT]

  protected def faucet: Faucet {
    type L = (DAT => Unit)
    type F = DAT
  }

  protected final def subscribe(aid: AID, eid: Option[EID], subscriber: PUB => Unit, strict: Boolean = true): Subscription = {
    val sid = newSubscriptionID(aid, eid)
    val latch = newLatch(strict)
      def proxySubscriber(data: DAT) {
        latch.await()
        subscriber(data.toPublish(sid))
      }
    subscribeToFaucet(aid, eid, sid, subscriber, proxySubscriber, latch)
  }

  private def subscribeToFaucet(aid: AID, eid: Option[EID], sid: SID, realSubscriber: PUB ⇒ Unit, proxySubscriber: DAT ⇒ Unit, latch: Latch): Subscription = {
    val subscription = faucet.subscribe(proxySubscriber, sid)
    try {
      store.connect(lookup(aid, eid)(_)) match {
        case Some(data) =>
          val msg = data.toPublish(sid)
          realSubscriber(msg)
        case _ => // Ignore
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
  protected val maxDataStoreLookupWaitOnStrict: FiniteDuration = 10.seconds

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
