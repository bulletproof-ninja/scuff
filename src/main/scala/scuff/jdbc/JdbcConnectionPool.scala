package scuff.jdbc

import java.sql.Connection
import scuff.concurrent.BoundedResourcePool
import scuff.concurrent.ResourcePool

/**
 * Bounded non-blocking JDBC connection pool.
 * @throws [[scuff.concurrent.ResourcePool.Exhausted]] when `maxConnections`
 * have been reached and all connections are unavailable.
 */
class JdbcConnectionPool(
    newConnection: => Connection,
    initialConnections: Int, maxConnections: Int,
    name: String = "JDBC connections")(
    implicit
    lifecycle: ResourcePool.Lifecycle[Connection] = DefaultConnectionLifecycle)
  extends BoundedResourcePool[Connection](
    newConnection, initialConnections, maxConnections, name)(implicitly, lifecycle)
  with ConnectionSource {

  protected def getConnection = newConnection

}
