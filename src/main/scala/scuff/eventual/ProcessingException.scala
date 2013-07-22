package scuff.eventual

class ProcessingException private (message: String, cause: Throwable) extends RuntimeException(message, cause)

object ProcessingException {
  def apply[A, B, C](cause: Throwable, txn: EventSource[A, B, C]#Transaction): ProcessingException = {
    val msg = "Failed processing \"%s\" stream %s/%d: %s%nMetadata: %s".format(txn.category, txn.streamId, txn.revision, cause.getMessage, txn.metadata.mkString(" | "))
    new ProcessingException(msg, cause)
  }
}