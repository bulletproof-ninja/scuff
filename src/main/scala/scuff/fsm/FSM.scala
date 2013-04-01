package scuff.fsm

trait Event

sealed class State {
  private[fsm] final val parent: Option[SuperState] = None
}

sealed trait Source extends State {
  protected[fsm] def onExit(evt: Event) {}
  protected[fsm] def onExit(evt: Event, payload: Any): Unit = onExit(evt)
}
sealed trait Target extends State {
  protected[fsm] def onEntry(evt: Event) {}
  protected[fsm] def onEntry(evt: Event, payload: Any): Unit = onEntry(evt)

}

class SuperState(name: String = "") extends Source {
  override def toString = if (name.length == 0) super.toString else name
}
class LeafState(name: String = "") extends Source with Target {
  override def toString = if (name.length == 0) super.toString else name
}
class FinalState(name: String = "") extends Target {
  override def toString = if (name.length == 0) super.toString else name
}

trait FSM extends SuperState {

  private[this] def assignParenthood(superState: SuperState): Unit = {
    val parent = Some(superState)
    superState.getClass.getDeclaredFields.withFilter(f ⇒ classOf[State].isAssignableFrom(f.getType)).foreach { field ⇒
      field.setAccessible(true)
      val subState = field.get(superState)
      new scuff.Surgeon(subState).setField('parent, parent)
      subState match {
        case ss: SuperState ⇒ assignParenthood(ss)
        case _ ⇒ // Ignore
      }
    }
  }

  @annotation.tailrec
  private def stateMatch(checkState: State, current: Option[State]): Boolean = current match {
    case None ⇒ false
    case Some(current) ⇒
      if (current eq checkState) {
        true
      } else {
        stateMatch(checkState, current.parent)
      }
  }

  def is(state: State) = stateMatch(state, currState)
  def current = currState
  def isFinal() = currState match {
    case Some(_: FinalState) ⇒ true
    case _ ⇒ false
  }

  type Transition = ((Source, Event), Target)
  protected def transitions: Set[Transition]
  private var currState: Option[Target] = None
  private var transitionTable: Map[(Source, Event), Target] = _
  protected def start(initialState: LeafState) = {
    transitionTable = transitions.toMap
    assignParenthood(this)
    currState = Option(initialState)
  }
  def apply(evt: Event, payload: Any = null) = currState match {
    case None ⇒ throw new IllegalStateException("State machine not started yet")
    case Some(source: Source) ⇒
      val target = transition(source, evt)
      source.onExit(evt, payload)
      currState = Some(target)
      target.onEntry(evt, payload)
    case _ ⇒ throw new IllegalStateException("State machine is finalized")
  }
  @annotation.tailrec
  private def transition(state: Source, evt: Event): Target = {
    val key = state -> evt
    transitionTable.get(key) match {
      case Some(toState) ⇒ toState
      case None ⇒ state.parent match {
        case None ⇒ throw new IllegalStateException("%s cannot handle %s".format(currState.get, evt))
        case Some(parent) ⇒ transition(parent, evt)
      }
    }
  }
}
