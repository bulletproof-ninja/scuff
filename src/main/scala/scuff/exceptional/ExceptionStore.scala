package scuff.exceptional

trait Storage {
  type ID
  case class ExceptionRef(stackTraceId: ID, message: Option[String])
  def getStackTraceId(t: Throwable): ID
  def saveIncident(exceptionChain: List[ExceptionRef], time: Long, metadata: Map[String, String])
}

class ExceptionStore(storage: Storage) {

  def store(t: Throwable): Unit = store(t, System.currentTimeMillis(), Map.empty)
  def store(t: Throwable, metadata: Map[String, String]): Unit = store(t, System.currentTimeMillis(), metadata)
  def store(t: Throwable, time: Long): Unit = store(t, time, Map.empty)
  def store(t: Throwable, time: Long, metadata: Map[String, String]): Unit = {
    val exceptionChain = registerChain(t)
    storage.saveIncident(exceptionChain, time, metadata)
  }

  private def registerChain(t: Throwable): List[storage.ExceptionRef] = {
    val causeChain = t.getCause match {
      case null ⇒ Nil
      case cause ⇒ registerChain(cause)
    }
    new storage.ExceptionRef(storage.getStackTraceId(t), Option(t.getMessage)) :: causeChain
  }

}
