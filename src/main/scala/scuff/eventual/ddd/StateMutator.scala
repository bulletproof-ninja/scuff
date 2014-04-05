package scuff.eventual.ddd

import scuff.ddd.DomainEvent
import scuff.ddd.DomainEventHandler

/**
 * This class encapsulates and transforms domain state,
 * based on events.
 */
trait StateMutator[EVT <: DomainEvent, S] extends DomainEventHandler[EVT] {
  /** Current state. */
  def state: S
}
