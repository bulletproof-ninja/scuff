package scuff.ddd

trait DomainEvent extends scuff.TypeVersioned with Serializable {
  def eventName = getClass.getSimpleName
}

trait DomainEventHandler[EVT <: DomainEvent] {
  def apply(evt: EVT)
}
