package scuff.eventual.util

import scuff.eventual._
import com.mongodb._
import scuff.Mongolia._
import concurrent.duration._
import java.util.Date
import scala.annotation.implicitNotFound
import scuff.Timestamp

/**
 * Generate state and keep track of handled [[scuff.eventual.EventSource#Transaction]]s, so process can resume
 * after shutdown.
 * Uses this collection format:
 * {{{
 * {  _id: <stream>,
 *    _rev: 123, // Latest revision
 *    _time: <timestamp>, // Transaction timestamp
 * }
 * }}}
 * @param dbColl MongoDB collection. Set whatever WriteConcern is appropriate before passing
 */
@implicitNotFound("Cannot find implicit Codec for ID <=> BsonValue")
final class MongoStateGenerator[ID](dbColl: DBCollection)(implicit idCdc: scuff.Codec[ID, BsonValue]) {
  
  def lastTimestamp: Option[Long] = {
    dbColl.find(obj("_time":=$exists(true)), obj("_id" := EXCLUDE, "_time" := INCLUDE)).last("_time").map { doc ⇒
      doc("_time").as[Long]
    }
  }

  private[this] final val RevReadFields = obj("_id" := EXCLUDE, "_rev" := INCLUDE)

  def nextExpectedRevision(streamId: ID): Int = {
    dbColl.findOne(obj("_id" := streamId), RevReadFields) match {
      case null ⇒ 0
      case doc ⇒ doc.getAs[Int]("_rev") + 1
    }
  }

  /**
   * Update stream and mark revision as processed.
   * @param streamId Transaction stream id
   * @param revision Transaction stream revision
   * @param time Transaction timestamp
   * @param update Optional update object.
   * @param listKey Additional key, only to be used for updating a specific list element
   */
  def commitRevision(streamId: ID, revision: Int, time: Long, update: DBObject = obj(), listKey: BsonProp = null) =
    pleaseUpdate(streamId, Some(revision -> time), update, listKey)

  private def pleaseUpdate(streamId: ID, revTime: Option[(Int, Long)], update: DBObject, listKey: BsonProp) {
    val key = obj("_id" := streamId)
    if (listKey != null) key.add(listKey)
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

  /**
   * Update stream state, but do not mark with revision.
   */
  def updateState(streamId: ID, update: DBObject, listKey: BsonProp = null) = pleaseUpdate(streamId, None, update, listKey)

}
