package scuff.eventual

class ProcessingException private (message: String, cause: Throwable) extends RuntimeException(message, cause)

object ProcessingException {
  def apply[A, B, C](cause: Throwable, txn: EventSource[A, B, C]#Transaction): ProcessingException = {
    val msg = s"Failed processing ${txn.category} stream ${txn.streamId}/${txn.revision}: ${cause.getMessage}${compat.Platform.EOL}Metadata: ${txn.metadata.mkString(" | ")}"
    new ProcessingException(msg, cause)
  }
}