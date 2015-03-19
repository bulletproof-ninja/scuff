package scuff.redis

import scuff.DistributedLocking._
import scuff.Codec
import _root_.redis.clients.jedis._
import scuff.Timestamp
import java.net.InetSocketAddress

class RedisLockStore[K](conn: CONNECTION)(implicit codec: Codec[K, String]) extends LockStore[K] {
  private def connection[T](thunk: Jedis => T): T = conn(thunk).asInstanceOf[T]
  private[this] val parser = new org.boon.json.JsonParserFactory().createFastObjectMapperParser()
  private[this] val hashName = getClass.getName
  protected def hash = hashName

  lazy val name: String = connection { conn =>
    val client = conn.getClient
    s"${client.getHost}:${client.getPort}/db${client.getDB}/$hash"
  }

  /**
   * Lock key for address. Returns `None` if successful.
   * If lock fails, the lock-holding info will be returned.
   */
  def writeLock(key: K, info: LockInfo): Option[LockInfo] = {
    val keyStr = codec.encode(key)
    val json = toJson(info)
    connection { c =>
      val existing = c.pipeline { pl =>
        val existing = pl.hget(hash, keyStr)
        pl.hsetnx(hash, codec.encode(key), json)
        existing
      }
      Option(existing.get).map(toLockInfo)
    }
  }
  /** Unlock key for address. */
  def removeLock(key: K, info: LockInfo) {
    val keyStr = codec.encode(key)
    val expected = toJson(info)
    connection { c =>
      c.watch(hash)
      val actual = c.hget(hash, keyStr)
      if (expected == actual) {
        val result = c.transaction { txn =>
          txn.hdel(hash, keyStr)
        }
        if (result.isEmpty) {
          removeLock(key, info)
        }
      }
    }
  }

  private def toJson(info: LockInfo) =
    s"""{"socket":{"host":${info.socketAddress.getHostName},"port":${info.socketAddress.getPort}},"since":"${info.since}","process":"${info.process}"}"""
  private def toLockInfo(json: String) = {
    val map = parser.parseMap(json)
    val process = String valueOf map.get("process")
    val since = Timestamp.parseISO(String valueOf map.get("since")) getOrElse new Timestamp
    val socket = map.get("socket").asInstanceOf[java.util.Map[String, Object]]
    val host = String valueOf socket.get("host")
    val port = socket.get("port").asInstanceOf[Number].intValue
    val socketAddress = new InetSocketAddress(host, port)
    new LockInfo(socketAddress, since, process)
  }

  def isLocked(key: K): Option[LockInfo] = connection(c => Option(c.hget(hash, codec encode key)).map(toLockInfo))

}
