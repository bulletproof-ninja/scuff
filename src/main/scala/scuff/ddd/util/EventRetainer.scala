package scuff.ddd.util

import scuff.ddd._
/**
 * This trait extends the [[DomainStateMutator]]
 * to retain all applied events.
 */
trait EventRetainer[EVT <: DomainEvent, S] extends StateMutator[EVT, S] {
  private[this] var events: List[EVT] = Nil
  abstract override def apply(evt: EVT) {
    super.apply(evt)
    events ::= evt
  }
  def appliedEvents: List[_ <: EVT] = events.reverse
}
