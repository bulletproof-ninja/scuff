package scuff.eventual.util

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
final class MongoStreamTracker[ID](
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
   * @param moreKey Additional key, only to be used for updating a specific list element
   */
  def markAsProcessed(streamId: ID, revision: Int, time: Long, update: DBObject = obj(), moreKey: BsonProp = null) =
    pleaseUpdate(streamId, Some(revision -> time), update, Option(moreKey))

  private def pleaseUpdate(streamId: ID, revTime: Option[(Int, Long)], update: DBObject, moreKey: Option[BsonProp]) {
    val key = obj("_id" := streamId)
    moreKey.foreach(p ⇒ key.add(p))
    val doUpsert = revTime.map {
      case (revision, time) ⇒
        update("$set") match {
          case v: Value ⇒
            v.as[DBObject].add("_rev" := revision, "_time" := new Date(time))
          case _ ⇒
            update.add($set("_rev" := revision, "_time" := new Date(time)))
        }
        revision == 0L
    }.getOrElse(true)

    val res = if (doUpsert) {
      // Use `upsert` because it allows modifiers in the `update` object, which `insert` doesn't
      dbColl.upsert(key, update)
    } else {
      dbColl.update(key, update)
    }
    if (res.getN == 0) {
      throw new IllegalStateException(s"Update $update failed for key $key.")
    }
  }

  def update(streamId: ID, update: DBObject, moreKey: BsonProp = null) = pleaseUpdate(streamId, None, update, Option(moreKey))

}
