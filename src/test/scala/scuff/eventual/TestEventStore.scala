package scuff.eventual

import org.junit._
import org.junit.Assert._
import scuff.eventual._
import scuff.eventual.util.InMemoryEventStore
import concurrent._
import duration._
import scala.util._
import scuff.eventual.ddd.EventHandler
import scuff.eventual.ddd.StateMutator
import scuff.eventual.ddd.EventHandler
import scuff.eventual.ddd.MapSnapshots
import scuff.eventual.ddd.EventStoreRepository
import scuff.Threads
import scuff.Clock
import language.implicitConversions
import scuff.Surgeon
import scuff.ByteOutputStream
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import scuff.ByteInputStream

abstract class TestEventStore {

  case class User(name: String, age: Int)

  sealed trait Event
  object Event {
    case class NameChanged(newName: String) extends Event
    case class AgeChanged(newAge: Int) extends Event
  }

  private[this] val es: EventStore[Symbol, Event, String] = null

  @Before
  def setup {
    new Surgeon(this).set('es, new InMemoryEventStore()(Threads.PiggyBack))
  }

  @Test
  def serialization {
    val wallClock = System.currentTimeMillis
    val txn = new es.Transaction(99, "USER", 'id12, 42, Map("wallClock" -> wallClock.toString), List(Event.AgeChanged(100), Event.NameChanged("Hansi")))
    val out = new ByteOutputStream
    val objOut = new ObjectOutputStream(out)
    val writeMethod = txn.getClass.getDeclaredMethod("writeObject", objOut.getClass)
    writeMethod.setAccessible(true)
    writeMethod.invoke(txn, objOut)
    objOut.close()
    val bytes = out.toArray
    val objInp = new ObjectInputStream(new ByteInputStream(bytes))
    val readMethod = txn.getClass.getDeclaredMethod("readObject", objInp.getClass)
    readMethod.setAccessible(true)
    val outTxn = new es.Transaction(567, "m,nxcv,mnxzcv", 'x5675675675, -786433, Map.empty, Nil)
    readMethod.invoke(outTxn, objInp)
    assertEquals(0, objInp.available)
    assertEquals(txn, outTxn)
  }
}
