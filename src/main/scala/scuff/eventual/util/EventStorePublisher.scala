package scuff.eventual.util

trait EventStorePublisher[ID, EVT, CAT] extends scuff.eventual.EventStore[ID, EVT, CAT] {

  protected def publish(t: Transaction): Unit

}

