package scuff

import java.net._
import java.io._
import scala.util._
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReference

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

  class AlreadyLockedException(key: Any, val by: InetSocketAddress, val since: Timestamp, val alive: Boolean)
    extends Exception(s"Key $key is already locked by $by since $since. Status: ${if (alive) "ACTIVE" else "UNKNOWN"}")

  /** The distributed data store abstraction. */
  trait LockStore[K] {
    protected[DistributedLocking] case class ExistingLock(ip: InetSocketAddress, since: Timestamp)
    /** Lock key for address. */
    def lock(key: K, address: InetSocketAddress): Option[ExistingLock]
    /** Unlock key for address. */
    def unlock(key: K, address: InetSocketAddress)
  }

  /**
   *  Lock server, which handles socket communication,
   *  to demonstrate/verify liveness of held lock.
   *  Locks are reentrant, meaning that if the same lock key
   *  is used multiple times, only the last `release` will
   *  actually release the lock.
   */
  class LockServer[K](socketPort: => Int, bindAddress: InetAddress = InetAddress.getLocalHost) {

    private def bind(port: Int): Try[ServerSocket] = {
      val address = new InetSocketAddress(bindAddress, port)
      val server = new ServerSocket
      Try {
        server.bind(address)
        server
      }
    }

    private type ServerThread = (Thread, InetSocketAddress)
    private type LockKey = (K, LockStore[K])
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

    private def verifyExistingLock(address: InetSocketAddress): Boolean = {
      Try(new Socket(address.getAddress, address.getPort)) match {
        case Failure(_: ConnectException) => false
        case Failure(e) => throw e
        case Success(socket) =>
          socket.close()
          true
      }
    }

    private def removeLockEntry(key: K, store: LockStore[K]) = locks.synchronized {
      locks.remove(key->store)
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

    @throws[AlreadyLockedException]
    final def obtainLock(key: K)(implicit store: LockStore[K]): Try[Locked] = Try {
      locks.synchronized {
        val (thread, address) = getServerThread()
        val lockState = locks.getOrElseUpdate(key->store, new LockState(store, key, address))
        if (lockState.reentry == 0) { // New instance
          store.lock(key, address) match {
            case Some(existing) if existing.ip != address =>
              removeLockEntry(key, store)
              val alive = verifyExistingLock(existing.ip)
              throw new AlreadyLockedException(key, existing.ip, existing.since, alive)
            case _ =>
              new Locked(key, lockState)
          }
        } else { // Existing instance
          lockState.reentry += 1
          new Locked(key, lockState)
        }
      }
    }

    final class Locked(val key: K, lockState: LockState) {
      private[this] var released = false
      def release() = locks.synchronized {
        if (!released) {
          lockState.release()
          released = true
        }
      }
    }

    private class LockState(store: LockStore[K], key: K, address: InetSocketAddress) {
      var reentry = 0
      def release() = locks.synchronized {
        if (reentry == 0) {
          store.unlock(key, address)
          removeLockEntry(key, store)
        } else {
          reentry -= 1
        }
      }
    }

  }

}
