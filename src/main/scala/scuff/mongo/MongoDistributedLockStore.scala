package scuff.mongo

import scuff.DistributedLocking._
import com.mongodb.DBCollection
import scuff.Mongolia._
import java.net.InetAddress
import scuff.Codec
import java.net.InetSocketAddress
import scuff.Timestamp
import scala.util._
import com.mongodb.DuplicateKeyException
import com.mongodb.DBObject

final class MongoLockStore[ID](lockCollection: DBCollection)(implicit codec: Codec[ID, BsonValue]) extends LockStore[ID] {

  private def newDoc(key: ID, record: Record) = obj(
    "_id" := key,
    "ip" := record.socketAddress.getAddress.getHostAddress,
    "port" := record.socketAddress.getPort,
    "jvm" := record.jvm,
    "since" := record.since
  )

  private def toRecord(doc: DBObject): Record = {
    val host = InetAddress.getByName(doc("ip").as[String])
    val port = doc("port").as[Int]
    val since = doc("since").as[Timestamp]
    val address = new InetSocketAddress(host, port)
    val jvm = doc("jvm").as[String]
    Record(address, since, jvm)
  }

  def isLocked(key: ID): Option[Record] = lockCollection.findOpt(_id(key)).map(toRecord)

  final def writeLock(key: ID, record: Record): Option[Record] = {
    val lockDoc = newDoc(key, record)
    Try(lockCollection.safeInsert(lockDoc)) match {
      case Failure(_: DuplicateKeyException) =>
        lockCollection.findOpt(_id(key)) match {
          case None =>
            writeLock(key, record) // Try again
          case Some(doc) =>
            Some(toRecord(doc))
        }
      case Failure(other) => throw other
      case Success(_) => None
    }
  }

  def removeLock(key: ID, record: Record) {
    lockCollection.remove(newDoc(key, record))
  }

}
