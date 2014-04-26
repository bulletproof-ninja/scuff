package scuff.eventual

import scala.util.control.NoStackTrace

class ProcessingException private[eventual] (message: String, cause: Throwable = null) extends RuntimeException(message, cause)

object ProcessingException {
  def apply[A, B, C](cause: Throwable, txn: EventSource[A, B, C]#Transaction): ProcessingException = {
    val msg = s"Failed processing ${txn.category} ${txn.streamId} rev ${txn.revision}: ${cause.getMessage}${compat.Platform.EOL}Metadata: ${txn.metadata.mkString(" | ")}"
    new ProcessingException(msg, cause)
  }

  def outOfSequence[A, B, C](txn: EventSource[A, B, C]#Transaction, expectedRevision: Int): OutOfSequenceException = {
    val msg = s"Failed processing ${txn.category} ${txn.streamId} rev ${txn.revision}: Expected revision $expectedRevision"
    new OutOfSequenceException(msg, expectedRevision)
  }
}

class OutOfSequenceException private[eventual] (message: String, val expectedRevision: Int) 
  extends ProcessingException(message, null)
