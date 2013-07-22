package scuff.exceptional

import scuff.Clock
import java.util.Date

trait Storage {
  type ID
  case class ExceptionRef(stackTraceId: ID, message: Option[String])
  def getStackTraceId(t: Throwable): ID
  def saveIncident(exceptionChain: List[ExceptionRef], timestamp: Date, metadata: Map[String, String])
}

class ExceptionStore(storage: Storage)(implicit clock: Clock) {

  def store(t: Throwable): Unit = store(t, clock.timestamp, Map.empty)
  def store(t: Throwable, metadata: Map[String, String]): Unit = store(t, clock.timestamp, metadata)
  def store(t: Throwable, time: Date): Unit = store(t, time, Map.empty)
  def store(t: Throwable, time: Date, metadata: Map[String, String]): Unit = {
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
