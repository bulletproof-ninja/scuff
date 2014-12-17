package scuff

import java.net._
import java.io._
import scala.util._
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicBoolean

/**
 * Basic distributed locking mechanism, that relies on a
 * distributed storage back-end (typically a database) to
 * ensure distributed exclusivity.
 * This is meant for longer term process locking and as such
 * does not include the normal locking semantics, such as
 * `wait`/`await`, but instead fails fast. A lock is either
 * obtained or not.
 */
object DistributedLocking {

  final class AlreadyLocked(val by: InetAddress, val since: Timestamp, val alive: Boolean, val jvm: String, val db: String)
  final class Locked[K](val key: K, lockState: LockServer[K]#LockState) {
    private[this] val released = new AtomicBoolean(false)
    def release() = {
      if (released.compareAndSet(false, true)) {
        lockState.releaseLock()
      }
    }
  }

  case class Record(socketAddress: InetSocketAddress, since: Timestamp, jvm: String)

  /** The distributed data store abstraction. */
  trait LockStore[K] {
    def dbName: String
    /**
     * Lock key for address. Returns `None` if successful.
     * If lock fails, the lock-holding record will be returned.
     */
    def writeLock(key: K, rec: Record): Option[Record]
    /** Unlock key for address. */
    def removeLock(key: K, record: Record)

    def isLocked(key: K): Option[Record]
  }

  /**
   *  Lock server, which handles socket communication,
   *  to demonstrate/verify liveness of held lock.
   *  Locks are reentrant, meaning that if the same lock key
   *  is used multiple times, only the last `release` will
   *  actually release the lock.
   */
  class LockServer[K](socketPort: => Int, bindAddress: InetAddress = InetAddress.getLocalHost) {

    private def newRecord(ip: InetSocketAddress) = {
      val jvm = java.lang.management.ManagementFactory.getRuntimeMXBean.getName
      new Record(ip, new Timestamp, jvm)
    }

    private def bind(port: Int): Try[ServerSocket] = {
      val address = new InetSocketAddress(bindAddress, port)
      val server = new ServerSocket
      Try {
        server.bind(address)
        server
      }
    }

    private type LockKey = (K, LockStore[K])
    private type ServerThread = (Thread, InetSocketAddress)
    private[this] var serverThread: Option[ServerThread] = None
    private[this] val locks = collection.mutable.Map[LockKey, LockState]()

    private def startServerSocket(prev: Option[(Int, BindException)] = None): ServerThread = {
      val port = socketPort
      val server = prev match {
        case Some((prevPort, ioe)) =>
          if (prevPort == port) {
            throw ioe
          } else {
            bind(port)
          }
        case None => bind(port)
      }
      server match {
        case Failure(ioe: BindException) => startServerSocket(Some(port -> ioe))
        case Failure(e) => throw e
        case Success(server) =>
          val thread = Threads.daemonFactory(getClass.getName) newThread new Runnable {
            def run = do {
              val client = server.accept()
              Try(client.close)
            } while (!Thread.currentThread.isInterrupted)
          }
          thread.start()
          thread -> server.getLocalSocketAddress.asInstanceOf[InetSocketAddress]
      }
    }

    private def verifyExistingServer(address: InetSocketAddress): Boolean = {
      Try(new Socket(address.getAddress, address.getPort)) match {
        case Failure(_: ConnectException) => false
        case Failure(_: NoRouteToHostException) => false
        case Failure(e) => throw e
        case Success(socket) =>
          socket.close()
          true
      }
    }

    private def removeLockEntry(key: K, store: LockStore[K]) = locks.synchronized {
      locks.remove(key -> store)
      if (locks.isEmpty) {
        serverThread.foreach(_._1.interrupt)
        serverThread = None
      }
    }

    private def getServerThread(): ServerThread = locks.synchronized {
      serverThread match {
        case None =>
          val server = startServerSocket()
          serverThread = Some(server)
          server
        case Some(st) => st
      }
    }

    final def verifyLocked(store: LockStore[K])(key: K): Option[AlreadyLocked] = {
      store.isLocked(key).map { existing =>
        val alive = verifyExistingServer(existing.socketAddress)
        new AlreadyLocked(existing.socketAddress.getAddress, existing.since, alive, existing.jvm, store.dbName)
      }
    }

    final def obtainLock(store: LockStore[K])(key: K): Either[AlreadyLocked, Locked[K]] = {
      locks.synchronized {
        val (thread, address) = getServerThread()
        val record = newRecord(address)
        val lockState = locks.getOrElseUpdate(key -> store, new LockState(store, key, record))
        if (lockState.reentry == 0) { // New instance
          store.writeLock(key, record) match {
            case Some(Record(socketAddr, since, jvm)) if socketAddr != address =>
              removeLockEntry(key, store)
              val alive = verifyExistingServer(socketAddr)
              Left(new AlreadyLocked(socketAddr.getAddress, since, alive, jvm, store.dbName))
            case _ =>
              Right(new Locked(key, lockState))
          }
        } else { // Existing instance
          lockState.reentry += 1
          Right(new Locked(key, lockState))
        }
      }
    }

    private[DistributedLocking] class LockState(store: LockStore[K], key: K, record: Record) {
      var reentry = 0
      Runtime.getRuntime() addShutdownHook new Thread {
        override def run = locks.synchronized {
          reentry = 0
          releaseLock()
        }
      }
      def releaseLock() = locks.synchronized {
        if (reentry == 0) {
          store.removeLock(key, record)
          removeLockEntry(key, store)
        } else {
          reentry -= 1
        }
      }
    }

  }

}

class DistributedLocking[K](server: DistributedLocking.LockServer[K], store: DistributedLocking.LockStore[K]) {
  def obtainLock(key: K) = server.obtainLock(store)(key)
  def verifyLock(key: K) = server.verifyLocked(store)(key)
}
