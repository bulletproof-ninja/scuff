package scuff.ddd

import java.util.concurrent.{ CountDownLatch, LinkedBlockingQueue, TimeUnit }

import scala.collection.immutable.{ Seq => ISeq }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import org.junit.Assert._
import org.junit.Test

import scuff.ddd.util.MapRepository

class TestRepository {

  private def withLatch(count: Int)(thunk: CountDownLatch => Unit) {
    val latch = new CountDownLatch(count)
    thunk(latch)
    assertTrue(latch.await(5, TimeUnit.SECONDS))
  }

  case class Customer(name: String, postCode: String)

  @Test
  def `insert success` {
    withLatch(2) { latch =>
      val repo = new MapRepository[Int, Customer]
      val hank = Customer("Hank", "12345")
      repo.insert(5, hank).foreach { rev =>
        assertEquals(0, rev)
        latch.countDown()
        repo.update(5, 0) {
          case (customer, rev) =>
            assertEquals(0, rev)
            latch.countDown()
            Future successful customer
        }
      }
    }
  }

  sealed trait VeryBasicEvent
  case object CustomerUpdated extends VeryBasicEvent
  case object CustomerCreated extends VeryBasicEvent

  @Test
  def `event publishing` {
    case class Notification(id: Int, revision: Int, events: ISeq[VeryBasicEvent])
    val notifications = new LinkedBlockingQueue[Notification]
    val repo = new EventEmittingRepository[Int, Customer, VeryBasicEvent](new MapRepository, global) {
      type Event = VeryBasicEvent
      def publish(id: Int, revision: Int, events: ISeq[Event], metadata: Map[String, String]) {
        notifications offer Notification(id, revision, events)
      }
    }
    val hank = Customer("Hank", "12345")
    repo.insert(5, hank -> List(CustomerCreated))
    val n1 = notifications.take
    assertEquals(5, n1.id)
    assertEquals(0, n1.revision)
    assertEquals(1, n1.events.size)
    assertEquals(CustomerCreated, n1.events.head)
    repo.update(5, 0) {
      case ((hank, _), rev) =>
        Future successful hank.copy(name = "Hankster") -> List(CustomerUpdated)
    }
    val n2 = notifications.take
    assertEquals(5, n2.id)
    assertEquals(1, n2.revision)
    assertEquals(1, n2.events.size)
    assertEquals(CustomerUpdated, n2.events.head)

    withLatch(1) { latch =>
      repo.load(5).foreach {
        case ((hank, _), rev) =>
          assertEquals(1, rev)
          assertEquals("Hankster", hank.name)
          latch.countDown()
      }
    }
  }

}
