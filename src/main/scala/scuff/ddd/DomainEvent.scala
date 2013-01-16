package scuff.ddd

trait DomainEvent extends scuff.Version with Serializable {
  def eventName = getClass.getSimpleName
}

trait DomainEventHandler[EVT <: DomainEvent] {
  def apply(evt: EVT)
}
