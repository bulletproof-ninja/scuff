package scuff

/**
 * Domain-driven development, with insistence
 * on domain events. This means that any change
 * to the aggregate root *must* result in at least
 * one event. If no events are produced, it's considered
 * unchanged.
 */
package object ddd {
  implicit def defaultAggrRoot[AR <: AggregateRoot](implicit domainEvt: DomainEvt[AR#EVT]) = new DefaultAggrRoot[AR]()(domainEvt)
  implicit def defaultDomainEvt[EVT <: DomainEvent] = new JavaSerializer[EVT] with DefaultDomainEvt[EVT]
}
