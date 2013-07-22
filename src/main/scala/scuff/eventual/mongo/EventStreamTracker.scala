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
 *    _rev: 123, // Latest revision
 *    _time: <timestamp>, // Transaction timestamp
 * }
 * ```
 * @param dbColl MongoDB collection. Set whatever WriteConcern is appropriate before passing
 * @param clockSkew Worst case clock skew in a sharded environment. Used for resuming without dropping transactions. Defaults to 2 seconds.
 */
final class EventStreamTracker[ID](dbColl: DBCollection, clockSkew: Duration = 2.seconds)(implicit idCdc: scuff.Codec[ID, BsonValue]) {

  def resumeFrom: Option[scuff.Timestamp] = {
    dbColl.find(obj(), obj("_id" := EXCLUDE, "_time" := INCLUDE)).last("_time").map { doc ⇒
      val time = doc("_time").as[Long]
      new scuff.Timestamp(time - clockSkew.toMillis)
    }
  }

  private[this] final val RevReadFields = obj("_id" := EXCLUDE, "_rev" := INCLUDE)

  def expectedRevision(streamId: ID): Long = {
    dbColl.findOne(obj("_id" := streamId), RevReadFields) match {
      case null ⇒ 0
      case doc ⇒ doc.getAs[Long]("_rev")
    }
  }

  /**
   * Mark stream/revision as processed.
   * @param streamId Transaction stream id
   * @param revision Transaction stream revision
   * @param time Transaction timestamp
   * @param save Optional save data.
   * Should not contain fields "_id", "_rev", "_time"
   * as those will be overwritten anyway.
   */
  def markAsProcessed(streamId: ID, revision: Long, time: scuff.Timestamp, save: DBObject = obj()) {
    save.add("_id" := streamId)
    save.put("_rev", revision) // Raw type
    save.put("_time", time) // Raw type
    dbColl.save(save)
  }
}
