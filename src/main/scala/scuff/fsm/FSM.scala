package scuff.fsm

object typed {
  sealed trait Source[-T] extends BaseState[T]
  sealed trait Target[-T] extends BaseState[T]
  class SuperState[T](name: String = "") extends Source[T] {
    override def toString = if (name.length == 0) super.toString else name
  }
  class State[T](name: String = "") extends Source[T] with Target[T] {
    override def toString = if (name.length == 0) super.toString else name
  }
  class FinalState[T](name: String = "") extends Target[T] {
    override def toString = if (name.length == 0) super.toString else name
  }
  trait FSM[T] extends typed.SuperState[T] {

    private[this] def assignParenthood(superState: typed.SuperState[T], ignore: collection.mutable.Set[Any]): Unit = {
      val parent = Some(superState)
      val baseStates = superState.getClass.getDeclaredFields.filter(f ⇒ classOf[BaseState[T]].isAssignableFrom(f.getType))
      baseStates.foreach { field ⇒
        field.setAccessible(true)
        val subState = field.get(superState)
        if (!ignore.contains(subState)) {
          new scuff.Surgeon(subState).set('parent, parent).set('assignedName, field.getName)
          ignore += subState
          subState match {
            case ss: typed.SuperState[T] ⇒ assignParenthood(ss, ignore)
            case _ ⇒ // Ignore
          }
        }
      }
    }

    @annotation.tailrec
    private def stateMatch(checkState: BaseState[T], current: Option[BaseState[T]]): Boolean = current match {
      case None ⇒ false
      case Some(current) ⇒ (current eq checkState) || stateMatch(checkState, current.parent)
    }

    def is(state: BaseState[T]) = stateMatch(state, currState)
    def current = currState
    def isFinal() = currState match {
      case Some(_: FinalState[_]) ⇒ true
      case _ ⇒ false
    }

    type Transition = ((typed.Source[T], Event), typed.Target[T])
    protected def transitions: Set[Transition]
    private var currState: Option[typed.Target[T]] = None
    private var transitionTable: Map[(typed.Source[T], Event), typed.Target[T]] = _
    protected def start(initialState: typed.State[T]) = {
      transitionTable = transitions.toMap
      require(transitionTable.size == transitions.size, "Duplicate State -> Event transition")
      assignParenthood(this, collection.mutable.Set(this))
      currState = Option(initialState)
    }
    def apply(evt: Event, payload: T = null.asInstanceOf[T]) = currState match {
      case None ⇒ throw new IllegalStateException("State machine not started yet")
      case Some(source: typed.Source[T]) ⇒
        val targetEvent = source.onEvent(evt, payload)
        val target = transition(source, targetEvent)
        if (source ne target) {
          currState = Some(target)
          target.onEvent(targetEvent, payload)
        }
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
private[fsm] sealed class BaseState[-T] {
  private[fsm] final val parent: Option[typed.SuperState[Any]] = None
  private[this] val assignedName: String = getClass.getSimpleName
  override def toString: String = parent.map(_.toString concat ".").getOrElse("") concat assignedName
  protected[fsm] def onEvent(evt: Event) {}
  protected[fsm] def onEvent(evt: Event, payload: T): Event = { onEvent(evt); evt }
}

/** Super state. Is expected to contain other states as `val`s. */
class SuperState(name: String = "") extends typed.SuperState[Any]

/** Leaf state. Is expected to not contain other sub states. */
class State(name: String = "") extends typed.State[Any]

/** Final state. */
class FinalState(name: String = "") extends typed.FinalState[Any]

/**
 * A finite state machine trait, which is itself a super state.
 */
trait FSM extends typed.FSM[Any]
