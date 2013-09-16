package scuff.eventual.mongo

import scuff.eventual._
import com.mongodb._
import scuff.Mongolia._
import concurrent.duration._

/**
 * Keep track of handled [[scuff.eventual.EventSource#Transaction]]s, so process can resume
 * after shutdown.
 * Uses this collection format:
 * {{{
 * {  _id: <stream>,
 *    _rev: 123, // Latest revision
 *    _time: <timestamp>, // Transaction timestamp
 * }
 * }}}
 * @param dbColl MongoDB collection. Set whatever WriteConcern is appropriate before passing
 * @param clockSkew Worst case clock skew in a sharded environment. Used for resuming without dropping transactions. Defaults to 2 seconds.
 */
final class EventStreamTracker[ID](
    dbColl: DBCollection,
    clockSkew: Duration = 2.seconds)(implicit idCdc: scuff.Codec[ID, BsonValue]) {

  def resumeFrom: Option[scuff.Timestamp] = {
    dbColl.find(obj(), obj("_id" := EXCLUDE, "_time" := INCLUDE)).last("_time").map { doc ⇒
      val time = doc("_time").as[Long]
      new scuff.Timestamp(time - clockSkew.toMillis)
    }
  }

  private[this] final val RevReadFields = obj("_id" := EXCLUDE, "_rev" := INCLUDE)

  def nextExpectedRevision(streamId: ID): Long = {
    dbColl.findOne(obj("_id" := streamId), RevReadFields) match {
      case null ⇒ 0
      case doc ⇒ doc.getAs[Long]("_rev") + 1L
    }
  }

  def lookup(streamId: ID): Option[(Long, DBObject)] = {
    dbColl.findOpt(obj("_id" := streamId), obj("_id" := EXCLUDE, "_time" := EXCLUDE)).map { doc ⇒
      doc.remove("_rev").as[Long] -> doc
    }
  }

  /**
   * Mark stream/revision as processed.
   * @param streamId Transaction stream id
   * @param revision Transaction stream revision
   * @param time Transaction timestamp
   * @param update Optional update object.
   */
  def markAsProcessed(streamId: ID, revision: Long, time: scuff.Timestamp, update: DBObject = obj()) {
    val key = obj("_id" := streamId)
    update.put("$set", new BasicDBObject("_rev", revision))
    update.put("$set", new BasicDBObject("_time", time))
    if (revision == 0L) {
      // Use `upsert` here because it allows modifiers in the `update` object, which `insert` doesn't
      dbColl.upsert(key, update)
    } else {
      dbColl.update(key, update)
    }
  }
}
