package scuff.fsm

object typed {
  sealed trait Source[-T] extends State {
  protected[fsm] def onExit(evt: Event) {}
    protected[fsm] def onExit(evt: Event, payload: T): Unit = onExit(evt)
}
  sealed trait Target[-T] extends State {
  protected[fsm] def onEntry(evt: Event) {}
    protected[fsm] def onEntry(evt: Event, payload: T): Unit = onEntry(evt)
}
  class SuperState[T](name: String = "") extends Source[T] {
  override def toString = if (name.length == 0) super.toString else name
}
  class LeafState[T](name: String = "") extends Source[T] with Target[T] {
  override def toString = if (name.length == 0) super.toString else name
}
  class FinalState[T](name: String = "") extends Target[T] {
  override def toString = if (name.length == 0) super.toString else name
}
  trait FSM[T] extends typed.SuperState[T] {

    private[this] def assignParenthood(superState: typed.SuperState[T]): Unit = {
    val parent = Some(superState)
    superState.getClass.getDeclaredFields.withFilter(f ⇒ classOf[State].isAssignableFrom(f.getType)).foreach { field ⇒
      field.setAccessible(true)
      val subState = field.get(superState)
      new scuff.Surgeon(subState).setField('parent, parent)
      subState match {
          case ss: typed.SuperState[T] ⇒ assignParenthood(ss)
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
      case Some(_: FinalState[_]) ⇒ true
    case _ ⇒ false
  }

    type Transition = ((typed.Source[T], Event), typed.Target[T])
  protected def transitions: Set[Transition]
    private var currState: Option[typed.Target[T]] = None
    private var transitionTable: Map[(typed.Source[T], Event), typed.Target[T]] = _
    protected def start(initialState: typed.LeafState[T]) = {
    transitionTable = transitions.toMap
    assignParenthood(this)
    currState = Option(initialState)
  }
    def apply(evt: Event, payload: T = null.asInstanceOf[T]) = currState match {
    case None ⇒ throw new IllegalStateException("State machine not started yet")
      case Some(source: typed.Source[T]) ⇒
      val target = transition(source, evt)
      source.onExit(evt, payload)
      currState = Some(target)
      target.onEntry(evt, payload)
    case _ ⇒ throw new IllegalStateException("State machine is finalized")
  }
  @annotation.tailrec
    private def transition(state: typed.Source[T], evt: Event): typed.Target[T] = {
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
}
/** Event marker trait. */
trait Event

/** General state representation. */
sealed class State {
  private[fsm] final val parent: Option[typed.SuperState[Any]] = None
}

/** Super state. Is expected to contain other states as `val`s. */
class SuperState(name: String = "") extends typed.SuperState[Any]

/** Leaf state. Is expected to not contain other sub states. */
class LeafState(name: String = "") extends typed.LeafState[Any]

/** Final state. */
class FinalState(name: String = "") extends typed.FinalState[Any]

/**
  * A finite state machine trait, which is itself a super state.
  */
trait FSM extends typed.FSM[Any]
