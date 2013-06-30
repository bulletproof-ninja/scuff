package scuff.ddd.util

import scuff.ddd._

/**
 * This class encapsulates and transforms domain state,
 * BUT does not (necessarily) collect events.
 */
trait StateMutator[EVT <: DomainEvent, S] extends DomainEventHandler[EVT] {
  /** Current state. */
  def state: S
}
