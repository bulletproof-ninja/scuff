package scuff.jdbc

import java.sql.Connection

import scuff.concurrent.{ BoundedResourcePool, ResourcePool }

/**
 * Bounded non-blocking JDBC connection pool.
 * @param newConnection New `Connection` function
 * @param minConnections Minimum number of connections to maintain in pool
 * @param maxConnection Maximum number of connections to allow in pool
 * @param name Optional pool name
 * @param lifecycle Type class for connection lifecycle behavior
 */
class JdbcConnectionPool(
  newConnection: => Connection,
  minConnections: Int, maxConnections: Int,
  name: String = "JDBC connections")(
  implicit
  lifecycle: ResourcePool.Lifecycle[Connection] = DefaultConnectionLifecycle)
extends BoundedResourcePool[Connection](
  newConnection, minConnections, maxConnections, name)(implicitly, lifecycle)
with ConnectionSource {

  @throws[ResourcePool.Exhausted]("when `maxConnections` have been reached and all connections are unavailable")
  protected def getConnection = newConnection

}
