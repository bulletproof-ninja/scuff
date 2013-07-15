package scuff.eventual.mongo

import scuff.eventual._
import com.mongodb._
import scuff.Mongolia._
import concurrent.duration._

/**
 * Keep track of handled [[Transaction]]s, so process can resume
 * after shutdown.
 * Uses this collection format:
 * ```
 * {  _id: <stream>,
 *    rev: 123, // Latest revision
 *    time: <timestamp>, // Transaction timestamp
 * }
 * ```
 * @param dbColl MongoDB collection. Set whatever WriteConcern is appropriate before passing
 * @param clockSkew Worst case clock skew in a sharded environment. Used for resuming without dropping transactions. Defaults to 2 seconds.
 */
final class EventStreamTracker[ID](dbColl: DBCollection, clockSkew: Duration = 2.seconds)(implicit idCdc: scuff.Codec[ID, BsonValue]) {

  def resumeFrom: Option[scuff.Timestamp] = {
    dbColl.find(obj(), obj("_id" := EXCLUDE, "time" := INCLUDE)).last("time").map { doc ⇒
      val time = doc("time").as[Long]
      new scuff.Timestamp(time - clockSkew.toMillis)
    }
  }

  private[this] final val RevReadFields = obj("_id" := EXCLUDE, "rev" := INCLUDE)

  def expectedRevision(streamId: ID): Long = {
    dbColl.findOne(obj("_id" := streamId), RevReadFields) match {
      case null ⇒ 0
      case doc ⇒ doc.getAs[Long]("rev")
    }
  }

  def markAsProcessed(streamId: ID, revision: Long, time: scuff.Timestamp) {
    val doc = obj("_id" := streamId)
    doc.put("rev", revision) // Raw type
    doc.put("time", time) // Raw type
    dbColl.save(doc)
  }
}
