package scuff.ddd.util

import scuff.ddd._
/**
 * This class encapsulates and transforms domain state AND collects the events that
 * causes the domain state transformations.
 */
class DomainEventCollector[EVT <: DomainEvent, S](stateMutator: DomainStateMutator[EVT, S]) extends DomainStateMutator[EVT, S] {
  private[this] var events: List[EVT] = Nil
  def apply(evt: EVT) {
    stateMutator(evt)
    events ::= evt
  }
  def appliedEvents: List[_ <: EVT] = events.reverse
  def state = stateMutator.state
}
