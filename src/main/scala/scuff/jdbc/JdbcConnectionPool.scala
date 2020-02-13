package scuff.jdbc

import java.sql.Connection
import scuff.concurrent.BoundedResourcePool
import scuff.concurrent.ResourcePool
import scuff.concurrent.ResourcePool.Exhausted

/**
 * Bounded non-blocking JDBC connection pool.
 * @param newConnection New `Connection` function
 * @param minConnections Minimum number of connections to maintain in pool
 * @param maxConnection Maximum number of connections to allow in pool
 * @param name Optional pool name
 * @param lifecycle Type class for connection lifecycle behavior
 */
@throws[Exhausted]("when `maxConnections` have been reached and all connections are unavailable")
class JdbcConnectionPool(
  newConnection: => Connection,
  minConnections: Int, maxConnections: Int,
  name: String = "JDBC connections")(
  implicit
  lifecycle: ResourcePool.Lifecycle[Connection] = DefaultConnectionLifecycle)
extends BoundedResourcePool[Connection](
  newConnection, minConnections, maxConnections, name)(implicitly, lifecycle)
with ConnectionSource {

  protected def getConnection = newConnection

}
