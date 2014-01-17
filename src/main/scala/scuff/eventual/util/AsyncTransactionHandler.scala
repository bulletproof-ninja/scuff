package scuff.eventual.util

import scuff.eventual._

/**
 * Asynchronous [[scuff.eventual.EventSource#Transaction]] handler.
 * NOTICE: This trait supports [[HashBasedSerialExecutionContext]]
 * to ensure predictable thread execution per stream.
 */
trait AsyncTransactionHandler[ID, EVT, CAT] extends (EventSource[ID, EVT, CAT]#Transaction ⇒ Unit) {

  /**
   * Execution context.
   * Supports `HashBasedSerialExecutionContext`.
   */
  protected def asyncTransactionCtx: concurrent.ExecutionContext

  private type TXN = EventSource[ID, EVT, CAT]#Transaction

  @inline private def callSuper(txn: TXN) = try {
    super.apply(txn)
  } catch {
    case t: Throwable ⇒ asyncTransactionCtx.reportFailure(ProcessingException(t, txn))
  }

  abstract override def apply(txn: TXN) {
    asyncTransactionCtx execute new Runnable {
      override def hashCode = txn.streamId.hashCode
      def run = callSuper(txn)
    }

  }
}