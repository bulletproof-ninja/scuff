package scuff.es.util

import scuff._
import scuff.Mongolia._
import com.mongodb._
import org.bson.types._
import java.util.Date
import scuff.es.DuplicateRevisionException
import scuff.es.EventStore

private object MongoEventStore {
  final val OrderById_asc = obj("_id" := ASC)
  final val OrderByTime_asc = obj("time" := ASC)
  final val OrderByRevision_asc = obj("key.revision" := ASC)
  final val OrderByRevision_desc = obj("key.revision" := DESC)

  def toBigInt(dbo: ObjectId) = BigInt(dbo.toByteArray)
  def toObjectId(bi: BigInt) = {
    var biArray = if (bi > 0) bi.toByteArray else new Array[Byte](12)
    if (biArray.length > 12) {
      throw new IllegalArgumentException("Value too large for ObjectId: " + bi)
    } else if (biArray.length < 12) {
      val temp = biArray
      biArray = new Array[Byte](12)
      System.arraycopy(temp, 0, biArray, 12 - temp.length, temp.length)
    }
    new ObjectId(biArray)
  }

  def ensureIndicies(coll: RichDBCollection): RichDBCollection = {
    coll.ensureUniqueIndex("key.stream" := ASC, "key.revision" := ASC)
    coll.ensureIndex("time")
    coll
  }

}

/**
  * Stores events, using this format:
  * {{{
  *   {
  *     _id: { "$oid" : "4ee104863aed01df303f3f27"} // ObjectId
  *     time: { "$date" : "2011-12-08T18:41:32.079Z"}, // Indexed. For reference purposes.
  *     key: { // Indexed
  *       stream: 34534, // Or whatever datatype ID is
  *       revision: 987, // Instance state revision
  *     }
  *     events: []
  *   }
  * }}}
  */
abstract class MongoEventStore[ID, EVT](dbColl: DBCollection)(implicit bsonConverter: ID ⇒ BsonValue, idExtractor: BsonValue ⇒ ID) extends EventStore[ID, EVT] {
  import MongoEventStore._

  private[this] val store = ensureIndicies(dbColl)

  protected def toDBObject(evt: EVT): DBObject
  protected def toEvent(dbo: DBObject): EVT
  protected def publish(t: Transaction): Unit

  @annotation.tailrec
  private def toBsonList(events: List[_ <: EVT], list: RichDBList = new RichDBList): RichDBList = events match {
    case Nil ⇒ list
    case head :: tail ⇒
      list += toDBObject(head)
      toBsonList(tail, list)
  }

  def replayStream[T](stream: ID)(callback: Iterator[Transaction] ⇒ T): T = {
    query("key.stream" := stream, OrderByRevision_asc, callback)
  }

  def replayStreamSince[T](stream: ID, sinceRevision: Long)(callback: Iterator[Transaction] ⇒ T): T = {
    val filter = obj(
      "key" := obj(
        "stream" := stream,
        "revision" := $gt(sinceRevision)))
    query(filter, OrderByRevision_asc, callback)
  }
  def replayStreamTo[T](stream: ID, toRevision: Long)(callback: Iterator[Transaction] ⇒ T): T = {
    val filter = obj(
      "key" := obj(
        "stream" := stream,
        "revision" := $lte(toRevision)))
    query(filter, OrderByRevision_asc, callback)
  }
  def replayStreamRange[T](stream: ID, revisionRange: collection.immutable.NumericRange[Long])(callback: Iterator[Transaction] ⇒ T): T = {
    val lowerBound = $gte(revisionRange.head)
    val upperBound = if (revisionRange.isInclusive) $lte("key.revision" := revisionRange.last) else $lt("key.revision" := revisionRange.last)
    val filter = obj(
      "key" := obj(
        "stream" := stream,
        "revision" := obj(lowerBound, upperBound)))
    val txnCallback = if (revisionRange.step == 1L) callback else (iter: Iterator[Transaction]) ⇒ callback(iter.filter(txn ⇒ revisionRange.contains(txn.revision)))
    query(filter, OrderByRevision_asc, txnCallback)
  }

  def replay[T](sinceTransactionID: BigInt)(txnHandler: Iterator[Transaction] ⇒ T): T = {
    val filter = obj("_id" := $gt(toObjectId(sinceTransactionID)))
    query(filter, OrderById_asc, txnHandler)
  }

  def replaySince[T](sinceTime: Date)(txnHandler: Iterator[Transaction] ⇒ T): T = {
    val filter = obj("time" := $gte(sinceTime))
    query(filter, OrderByTime_asc, txnHandler)
  }

  def record(streamID: ID, revision: Long, events: List[_ <: EVT]) {
    val oid = new ObjectId
    val timestamp = new scuff.Timestamp
    val doc = obj(
      "_id" := oid,
      "time" := timestamp,
      "key" := obj(
        "stream" := streamID,
        "revision" := revision),
      "events" := toBsonList(events))
    try {
      store.safeInsert(doc)
    } catch {
      case e: MongoException.DuplicateKey ⇒ throw new DuplicateRevisionException
    }
    publish(new Transaction(toBigInt(oid), timestamp, streamID, revision, events))
  }

  private def tryRecord(stream: ID, revision: Long, events: List[_ <: EVT]): Long = try {
    record(stream, revision, events)
    revision
  } catch {
    case _: DuplicateRevisionException ⇒ tryRecord(stream, revision + 1L, events)
  }

  def record(stream: ID, events: List[_ <: EVT]): Long = {
    val revision = store.find("key.stream" := stream).sort(OrderByRevision_desc).limit(1).nextOpt.map(_.apply("key.revision").as[Long]).getOrElse(-1L) + 1L
    tryRecord(stream, revision, events)
  }

  private def query[T](filter: DBObject, ordering: DBObject, handler: Iterator[Transaction] ⇒ T): T = {
    import collection.JavaConverters._
    val cursor = store.find(filter).sort(ordering)
    try {
      val iterator = (cursor: java.util.Iterator[DBObject]).asScala.map(toTransaction(_))
      handler(iterator)
    } finally {
      cursor.close()
    }
  }

  private def toTransaction(doc: RichDBObject): Transaction = {
    val transactionID = toBigInt(doc._id)
    val timestamp = doc("time").as[Timestamp]
    val (id, revision) = doc("key").as[DBObject].map { key ⇒ (key("stream").as[ID], key.getAs[Long]("revision")) }
    val events = doc("events").asSeq[DBObject].map(toEvent)
    new Transaction(transactionID, timestamp, id, revision, events.toList)
  }

}
