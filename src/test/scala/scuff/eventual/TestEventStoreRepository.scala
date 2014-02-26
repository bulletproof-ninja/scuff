package scuff.eventual

import org.junit._
import org.junit.Assert._
import scuff.ddd._
import scuff.eventual._
import scuff.eventual.util.InMemoryEventStore
import concurrent._
import duration._
import scala.util._
import ExecutionContext.Implicits.global
import scuff.eventual.ddd.EventHandler
import scuff.eventual.ddd.StateMutator
import scuff.eventual.ddd.EventHandler
import scuff.eventual.ddd.MapSnapshots
import scuff.eventual.ddd.EventStoreRepository
import scuff.Threads
import scuff.Clock
import language.implicitConversions

abstract class AbstractEventStoreRepositoryTest {

  implicit private val noMetadata = Map.empty[String, String]
  implicit def clock = Clock.System
  implicit def catConv(ar: Aggr) = ()
  implicit def idConv(id: String) = id

  var es: EventStore[String, AggrEvent, Unit] = _
  var repo: Repository[Aggr] = _

  private def doAsync(f: Promise[Any] ⇒ Unit) {
    val something = Promise[Any]
    f(something)
    Await.result(something.future, 15.seconds) match {
      case t: Throwable ⇒ throw t
      case Failure(t) ⇒ throw t
      case _ ⇒
    }
  }

  @Test
  def loadUnknownId = doAsync { done ⇒
    repo.load("Foo").onFailure {
      case e: UnknownIdException ⇒
        assertEquals("Foo", e.id)
        done.success(Unit)
    }
  }

  @Test
  def saveNew = doAsync { done ⇒
    val newFoo = Aggr.create("Foo")
    assertEquals(1, newFoo.events.size)
    repo.insert(newFoo).onSuccess {
      case _ ⇒ done.success(Unit)
    }
  }
  @Test
  def update = doAsync { done ⇒
    val insert = repo.insert {
      val newFoo = Aggr.create("Foo")
      newFoo(AddNewNumber(42))
      newFoo.events match {
        case AggrCreated() :: NewNumberWasAdded(n) :: Nil ⇒ assertEquals(42, n)
        case _ ⇒ fail("Event sequence incorrect: " + newFoo.events)
      }
      newFoo
    }
    val update1 = insert.flatMap {
      case _ ⇒
        repo.update("Foo", 0) { foo ⇒
          foo(AddNewNumber(42))
          assertEquals(0, foo.events.size)
          foo(AddNewNumber(99))
          assertEquals(1, foo.events.size)
        }
    }
    update1.onSuccess {
      case revision ⇒
        assertEquals(1L, revision)
        repo.load("Foo" -> 0).onSuccess {
          case foo ⇒
            assertEquals(0L, foo.revision.get)
            assertTrue(foo.numbers.contains(42))
            assertFalse(foo.numbers.contains(99))
            assertEquals(1, foo.numbers.size)
            repo.load("Foo").onSuccess {
              case foo ⇒
                assertEquals(1L, foo.revision.get)
                assertTrue(foo.numbers.contains(42))
                assertTrue(foo.numbers.contains(99))
                assertEquals(2, foo.numbers.size)
                done.success(Unit)
            }
        }
    }
  }
  @Test
  def `programmer error` = doAsync { done ⇒
    val aggrWithRevision = new Aggr("FooBar", new EventHandler(new AggrStateMutator), Some(42))
    repo.insert(aggrWithRevision).onComplete {
      case Failure(e: IllegalStateException) ⇒
        assertTrue(e.getMessage().contains("FooBar") && e.getMessage.contains("42"))
      case c ⇒
        fail("Should not happen: " + c)
    }
    val aggrWithNoEvents = new Aggr("FooBar", new EventHandler(new AggrStateMutator), None)
    repo.insert(aggrWithNoEvents).onComplete {
      case Failure(e: IllegalStateException) ⇒
        assertTrue(e.getMessage().contains("FooBar"))
      case c ⇒
        fail("Should not happen: " + c)
    }
    val aggrWithEventNoRevision = Aggr.create("FooBar")
    repo.insert(aggrWithEventNoRevision).onComplete {
      case Success(_) ⇒
        done.success(Unit)
      case c ⇒
        fail("Should not happen: " + c)
    }
  }
  @Test
  def `duplicate id` = doAsync { done ⇒
    repo.insert(Aggr.create("Baz")).onSuccess {
      case _ ⇒
        repo.insert(Aggr.create("Baz")).onFailure {
          case e: DuplicateIdException ⇒
            assertEquals("Baz", e.id)
            done.success(Unit)
        }
    }
  }

  @Test
  def `concurrent update` = doAsync { done ⇒
    val executor = java.util.concurrent.Executors.newScheduledThreadPool(16)
    val insFut = repo.insert(Aggr.create("Foo"))
    val map = new scuff.LockFreeConcurrentMap[Int, Future[Int]]
    val range = 0 to 250
    insFut.foreach { _ ⇒
      for (i ← range) {
        val runThis = new Runnable {
          def run {
            val fut = repo.update("Foo", 0) { foo ⇒
              foo(AddNewNumber(i))
            }
            map += i -> fut
          }
        }
        executor.schedule(runThis, 500, java.util.concurrent.TimeUnit.MILLISECONDS)
      }
      while (map.size < range.size) {}
      val revisions = map.map {
        case (i, rev) ⇒ Await.result(rev, Duration.Inf)
      }.toSeq.sorted
      done.complete(Try(assertEquals((1 to range.size).toSeq, revisions)))
    }
  }
  @Test
  def `noop update`() = doAsync { done ⇒
    repo.insert(Aggr.create("Foo")).onSuccess {
      case _ ⇒
        var twoPlusTwo = 0
        repo.update("Foo", 0) { foo ⇒
          twoPlusTwo = 2 + 2
        }.onSuccess {
          case storedRev ⇒
            assertEquals(4, twoPlusTwo)
            assertEquals(0, storedRev)
            done.success(Unit)
        }
    }
  }
}

class Aggr(val id: String, onEvent: EventHandler[AggrEvent, AggrState], val revision: Option[Int] = None) extends AggregateRoot {
  type EVT = AggrEvent
  type ID = String
  def events: List[_ <: EVT] = onEvent.events
  private def aggr = onEvent.state
  def apply(cmd: AddNewNumber) {
    if (!onEvent.state.numbers.contains(cmd.n)) {
      onEvent(NewNumberWasAdded(cmd.n))
    }
  }
  def numbers = aggr.numbers
}

object Aggr {
  def create(id: String): Aggr = {
    val mutator = new EventHandler(new AggrStateMutator)
    mutator(new AggrCreated)
    new Aggr(id, mutator)
  }
}

case class AddNewNumber(n: Int)

case class AggrState(numbers: Set[Int] = Set.empty)

sealed abstract class AggrEvent(val typeVersion: Short) extends DomainEvent
case class AggrCreated() extends AggrEvent(1)
case class NewNumberWasAdded(n: Int) extends AggrEvent(1)

class AggrStateMutator(var state: AggrState = null, concurrentEvents: List[AggrEvent] = Nil)
    extends StateMutator[AggrEvent, AggrState] {
  type S = AggrState
  type EVT = AggrEvent

  def apply(evt: AggrEvent) = {
    evt match {
      case AggrCreated() ⇒
        assertNull(state)
        state = new AggrState
      case NewNumberWasAdded(number) ⇒
        assertNotNull(state)
        val newNumbers = state.numbers + number
        state = state.copy(numbers = newNumbers)
    }
  }
}

class TestEventStoreRepositoryNoSnapshots extends AbstractEventStoreRepositoryTest {

  @Before
  def setup {
    es = new InMemoryEventStore[String, AggrEvent, Unit] {
      def clock = Clock.System
      def txn2cat(txn: Transaction) = ()
    }
    repo = new EventStoreRepository[String, Aggr, Unit] {
      def errHandler(t: Throwable) = throw t
      val eventStore = es
      type S = AggrState
      def newStateMutator(snapshotState: Option[S]) = new AggrStateMutator(snapshotState.getOrElse(null))
      def newAggregateRoot(id: String, revision: Int, state: S, concurrentUpdates: List[_ <: AggrEvent]) = {
        val collector = new EventHandler(new AggrStateMutator(state, concurrentUpdates))
        new Aggr(id, collector, Some(revision))
      }
    }
  }

}

class TestEventStoreRepositoryWithSnapshots extends AbstractEventStoreRepositoryTest {

  @Before
  def setup {
    es = new InMemoryEventStore[String, AggrEvent, Unit] {
      def clock = Clock.System
      def txn2cat(txn: Transaction) = ()
    }
    repo = new EventStoreRepository[String, Aggr, Unit] with MapSnapshots[String, Aggr, Unit] {
      def errHandler(t: Throwable) = throw t
      val eventStore = es
      type S = AggrState
      val snapshots = new scuff.LockFreeConcurrentMap[String, (S, Int)]
      def newStateMutator(snapshotState: Option[AggrState]) = new AggrStateMutator(snapshotState.getOrElse(null))
      def newAggregateRoot(id: String, revision: Int, state: S, concurrentUpdates: List[_ <: AggrEvent]) = {
        val collector = new EventHandler(new AggrStateMutator(state, concurrentUpdates))
        new Aggr(id, collector, Some(revision))
      }
    }
  }

}
