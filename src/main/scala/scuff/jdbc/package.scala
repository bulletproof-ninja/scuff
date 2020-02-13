package scuff

import java.sql._
import scuff.concurrent.ResourcePool

package jdbc {

  class DefaultConnectionLifecycle extends ResourcePool.Lifecycle[Connection] {
    def onCheckout(c: Connection): c.type = c
    def onReturn(c: Connection): c.type = c
    def onEviction(c: Connection): Unit = ()
    def evictOnFailure(e: Throwable): Boolean = e match {
      case _: SQLNonTransientConnectionException => true
      case e: SQLException => Option(e.getSQLState).exists(_ startsWith "08")
      case _ => false
    }
  }
}

package object jdbc {

  val DefaultConnectionLifecycle = new DefaultConnectionLifecycle

}
