package scuff

/**
 * Message interface for enabling dispatch to
 * callback interface.
 */
trait DoubleDispatch[CB <: { type RT }] {
  def dispatch(callback: CB): callback.RT
}

