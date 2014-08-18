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

class MongoLockStore[ID](lockCollection: DBCollection)(implicit codec: Codec[ID, BsonValue]) extends LockStore[ID] {

  private def newDoc(key: ID, address: InetSocketAddress) = obj(
    "_id" := key,
    "ip" := address.getAddress.getHostAddress,
    "port" := address.getPort
  )

  final def lock(key: ID, address: InetSocketAddress): Option[ExistingLock] = {
    val lockDoc = newDoc(key, address).add("since" := new Timestamp)
    Try(lockCollection.safeInsert(lockDoc)) match {
      case Failure(_: DuplicateKeyException) =>
        lockCollection.findOpt(_id(key)) match {
          case None =>
            lock(key, address) // Try again
          case Some(doc) =>
            val host = InetAddress.getByName(doc("ip").as[String])
            val port = doc("port").as[Int]
            val since = doc("since").as[Timestamp]
            val address = new InetSocketAddress(host, port)
            Some(ExistingLock(address, since))
        }
      case Failure(other) => throw other
      case Success(_) => None
    }
  }

  def unlock(key: ID, address: InetSocketAddress) {
    lockCollection.remove(newDoc(key, address))
  }

}
