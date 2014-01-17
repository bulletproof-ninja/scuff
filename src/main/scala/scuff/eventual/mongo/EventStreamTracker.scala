package scuff.eventual.mongo

import scuff.eventual._
import com.mongodb._
import scuff.Mongolia._
import concurrent.duration._
import java.util.Date

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

  def nextExpectedRevision(streamId: ID): Int = {
    dbColl.findOne(obj("_id" := streamId), RevReadFields) match {
      case null ⇒ 0
      case doc ⇒ doc.getAs[Int]("_rev") + 1
    }
  }

//  def lookup(streamId: ID): Option[(Int, DBObject)] = {
//    dbColl.findOpt(obj("_id" := streamId), obj("_id" := EXCLUDE, "_time" := EXCLUDE)).map { doc ⇒
//      doc.remove("_rev").as[Int] -> doc
//    }
//  }

  /**
   * Mark stream/revision as processed.
   * @param streamId Transaction stream id
   * @param revision Transaction stream revision
   * @param time Transaction timestamp
   * @param update Optional update object.
   */
  def markAsProcessed(streamId: ID, revision: Int, time: Long, update: DBObject = obj(), moreKey: BsonProp = null) {
    val key = obj("_id" := streamId)
    if (moreKey != null) {
      key.add(moreKey)
    }
    val $set = update("$set") match {
      case v: Value ⇒ v.as[DBObject]
      case _ ⇒ obj()
    }
    $set.add("_rev" := revision, "_time" := new Date(time))
    update.put("$set", $set)
    if (revision == 0L) {
      // Use `upsert` because it allows modifiers in the `update` object, which `insert` doesn't
      dbColl.upsert(key, update)
    } else {
      dbColl.update(key, update)
    }
  }
}
