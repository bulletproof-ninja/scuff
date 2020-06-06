package scuff

import concurrent.ResourcePool

import java.sql._

package jdbc {

  class DefaultConnectionLifecycle extends ResourcePool.Lifecycle[Connection] {
    def onCheckout(c: Connection): c.type = c
    def onReturn(c: Connection): c.type = c
    def onEviction(c: Connection): Unit = ()
    def evictOnFailure(e: Throwable): Boolean = e match {
      case _: SQLSyntaxErrorException =>
        // Syntax failure, connection is fine.
        false
      case _: SQLNonTransientConnectionException =>
        // "the connection operation that failed will not succeed if the operation is retried without the cause of the failure being corrected."
        true
      case _: SQLTransientException =>
        // "is thrown in situations where a previoulsy failed operation might be able to succeed when the operation is retried without any intervention by application-level functionality."
        false
      case _: SQLRecoverableException =>
        // "At a minimum, the recovery operation must include closing the current connection and getting a new connection."
        true
      case e: SQLException =>
        // Same as SQLNonTransientConnectionException, if true:
        Option(e.getSQLState).exists(_ startsWith "08")
      case _ =>
        // Don't evict on non-SQL exceptions
        false
    }
  }
}

package object jdbc {

  val DefaultConnectionLifecycle: ResourcePool.Lifecycle[Connection] =
    new DefaultConnectionLifecycle

}
