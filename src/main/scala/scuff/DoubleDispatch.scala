package scuff

/**
 * Message interface for enabling dispatch to
 * callback interface.
 */
trait DoubleDispatch[CB] {
  def dispatch(callback: CB)
}
