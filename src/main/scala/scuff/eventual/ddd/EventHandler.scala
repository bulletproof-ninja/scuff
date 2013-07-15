package scuff.eventual.ddd

import scuff.ddd._

/**
 * This class extends [[StateMutator]]
 * and retains all applied events.
 */
class EventHandler[EVT <: DomainEvent, S](sm: StateMutator[EVT, S]) extends StateMutator[EVT, S] {
  private[this] var _events: List[EVT] = Nil
  def apply(evt: EVT) {
    sm.apply(evt)
    _events ::= evt
  }
  def events: List[_ <: EVT] = _events.reverse
  def state = sm.state
}
