package scuff

/**
  * Message interface for enabling dispatch to
  * callback interface.
  */
trait DoubleDispatch {
  type Callback <: { type Return }
  def dispatch(callback: Callback): callback.Return
}
