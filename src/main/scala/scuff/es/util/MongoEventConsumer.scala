package scuff.es.util

import scuff.es._
import com.mongodb._
import scuff.Mongolia._
import concurrent.duration._

/**
 * Keep track of handled [[Transaction]]s, so process can resume
 * after shutdown.
 */
abstract class MongoEventConsumer[ID <% BsonValue, EVT, CAT <% BsonValue](dbColl: DBCollection)
    extends scuff.es.PersistentEventConsumer[ID, EVT, CAT] {

  /**
   * Worst case clock-skew, when database is sharded.
   */
  protected def clockSkew: Duration = 5.seconds
  protected def writeConcern: WriteConcern = WriteConcern.UNACKNOWLEDGED

  private[this] val syncLock = new AnyRef
  private[this] val revDocSave = obj()
  private[this] val revDocQry = obj()

  final def resumeFrom: Option[scuff.Timestamp] = syncLock.synchronized {
    dbColl.find(obj(), obj("_id" := EXCLUDE, "time" := INCLUDE)).first("time" := DESC).map { doc ⇒
      val time = doc("time").as[Long]
      new scuff.Timestamp(time - clockSkew.toMillis)
    }
  }

  final def lastProcessedRev(streamId: ID): Option[Long] = syncLock.synchronized {
    revDocQry += ("_id" := streamId)
    dbColl.findOne(revDocQry) match {
      case null ⇒ None
      case doc ⇒ Some(doc.getAs[Long]("rev"))
    }
  }

  protected def processNext(txn: TXN)

  final def consume(txn: TXN) = syncLock.synchronized {
    processNext(txn)
    revDocSave.add("_id" := txn.streamId) // Type conversion
    revDocSave.put("rev", txn.revision) // Raw type
    revDocSave.put("time", txn.timestamp) // Raw type
    dbColl.save(revDocSave, writeConcern)
  }

}
