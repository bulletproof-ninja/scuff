package scuff.jdbc

import java.sql.{ Connection, SQLWarning }
import scala.concurrent.blocking
import scala.util.Try
import javax.sql.{ DataSource, ConnectionPoolDataSource }

import java.sql.SQLException
import java.sql.SQLTransientException
import java.sql.SQLRecoverableException
import scuff.concurrent.ResourcePool
import scala.util.control.NonFatal

/**
 * Generic trait for providing a JDBC connection.
 * By default, any warnings will get thrown. This
 * behavior can be modified by either fixing the
 * cause of the warning(s), which is the recommended
 * approach, or, if a fix is infeasible, overriding the
 * `processWarnings` method.
 */
trait ConnectionProvider {
  protected def getConnection: Connection
  protected def prepare(conn: Connection, readOnly: Boolean): Connection = {
    conn setReadOnly readOnly
    conn setAutoCommit readOnly
    conn
  }
  protected def maxConnections: Int = Runtime.getRuntime.availableProcessors * 4
  protected def getConnection(readOnly: Boolean): Connection = prepare(getConnection, readOnly)
  protected def processWarnings(warnings: SQLWarning): Unit = throw warnings
  protected def useConnection[R](readOnly: Boolean)(thunk: Connection => R): R = blocking {
    val conn = getConnection(readOnly)
    try {
      val r = thunk(conn)
      Option(conn.getWarnings).foreach(processWarnings)
      r
    } finally Try(conn.close)
  }
  protected def forUpdate[R](thunk: Connection => R): R =
    useConnection(readOnly = false) { conn =>
      try {
        val r = thunk(conn)
        conn.commit()
        r
      } catch {
        case NonFatal(t) =>
          Try(conn.rollback())
          throw t
      }
    }
  protected def forQuery[R](thunk: Connection => R): R =
    useConnection(readOnly = true)(thunk)
}

trait DataSourceConnection extends ConnectionProvider {
  protected def dataSource: DataSource
  protected def getConnection: Connection = dataSource.getConnection
}

trait ConnectionPoolDataSourceConnection extends ConnectionProvider {
  protected def dataSource: ConnectionPoolDataSource
  protected def getConnection: Connection = dataSource.getPooledConnection.getConnection
}

/**
 *  Connection provider that uses a [[scuff.concurrent.ResourcePool]]
 *  to re-use connections.
 *  Can be used as-is, or `def newResourcePool` can be overridden for custom
 *  pool behavior, or `def pool` can be overridden to use an existing pool.
 */
trait ResourcePoolConnection extends ConnectionProvider {
  protected def minSize = 1
  protected def name = getClass.getSimpleName
  protected def lifecycle: ResourcePool.Lifecycle[Connection]
  private[this] lazy val _pool = newResourcePool(getConnection, lifecycle)
  protected def newResourcePool(
      init: => Connection, lifecycle: ResourcePool.Lifecycle[Connection]): ResourcePool[Connection] =
    new JdbcConnectionPool(init, minSize, maxConnections, name)(lifecycle)
  protected def pool: ResourcePool[Connection] = _pool
  override protected def useConnection[R](readOnly: Boolean)(thunk: Connection => R): R =
    pool.use { conn =>
      thunk(prepare(conn, readOnly))
    }
}

/**
 *  Connection provider that uses two [[scuff.concurrent.ResourcePool]]s,
 *  for keeping reads and writes separate.
 */
trait DualResourcePoolConnection extends ConnectionProvider {
  protected def minSize = 1
  protected def name = getClass.getSimpleName
  protected def lifecycle: ResourcePool.Lifecycle[Connection]
  private[this] lazy val _readPool = newResourcePool(readOnly = true, super.getConnection(readOnly = true), lifecycle)
  private[this] lazy val _writePool = newResourcePool(readOnly = false, super.getConnection(readOnly = false), lifecycle)
  protected def newResourcePool(
      readOnly: Boolean, init: => Connection,
      lifecycle: ResourcePool.Lifecycle[Connection]): ResourcePool[Connection] =
    new JdbcConnectionPool(init, minSize, maxConnections, s"$name, ${if (readOnly) "reads" else "writes"}")(lifecycle)
  protected def readPool: ResourcePool[Connection] = _readPool
  protected def writePool: ResourcePool[Connection] = _writePool
  override protected def useConnection[R](readOnly: Boolean)(thunk: Connection => R): R = {
    val pool = if (readOnly) readPool else writePool
    pool.use(thunk)
  }
}

trait Retry extends ConnectionProvider {

  protected def retryCount: Int

  protected def shouldRetry(e: SQLException): Boolean = e match {
    case _: SQLTransientException | _: SQLRecoverableException => true
    case _ => false
  }

  final override protected def useConnection[R](readOnly: Boolean)(thunk: Connection => R): R = {
    tryThunk(readOnly, retryCount, thunk)
  }
  private def tryThunk[R](readOnly: Boolean, retriesLeft: Int, thunk: Connection => R): R = {
    try {
      super.useConnection(readOnly)(thunk)
    } catch {
      case e: SQLException if retriesLeft > 0 && shouldRetry(e) =>
        tryThunk(readOnly, retriesLeft - 1, thunk)
    }
  }
}

trait ConnectionSource extends ConnectionProvider {
  override def forUpdate[R](thunk: Connection => R): R = super.forUpdate(thunk)
  override def forQuery[R](thunk: Connection => R): R = super.forQuery(thunk)
}
