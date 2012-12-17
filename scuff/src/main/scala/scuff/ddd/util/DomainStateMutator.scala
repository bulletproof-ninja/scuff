package scuff.ddd.util

import scuff.ddd._

/**
 * This class encapsulates and transforms domain state, 
 * BUT does not (necessarily) collect events.
 */
trait DomainStateMutator[EVT <: DomainEvent, S] extends DomainEventHandler[EVT] {
  def currentState: S
}
