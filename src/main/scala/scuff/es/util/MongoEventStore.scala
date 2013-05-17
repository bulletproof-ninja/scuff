package scuff.es.util

import scuff._
import scuff.Mongolia._
import com.mongodb._
import org.bson.types._
import java.util.Date
import scuff.es.DuplicateRevisionException
import scuff.es.EventStore

private object MongoEventStore {
  final val OrderByTime_asc = obj("time" := ASC)
  final val OrderByRevision_asc = obj("stream.rev" := ASC)
  final val OrderByRevision_desc = obj("stream.rev" := DESC)

  def ensureIndicies(coll: RichDBCollection): RichDBCollection = {
    coll.ensureUniqueIndex("stream.id" := ASC, "stream.rev" := ASC)
    coll.ensureIndex("time")
    coll.ensureIndex("category")
    coll
  }

}

/**
 * Stores events, using this format:
 * {{{
 *   {
 *     _id: ObjectId("4ee104863aed01df303f3f27"),
 *     time: { "$date" : "2011-12-08T18:41:32.079Z"}, // Indexed. For reference purposes.
 *     category: "FooBar",
 *     stream: { // Indexed
 *       id: 34534, // Stream identifier
 *       rev: 987, // Stream revision
 *     }
 *     events: []
 *   }
 * }}}
 */
abstract class MongoEventStore[ID, EVT, CAT](dbColl: DBCollection)(implicit idConv: Transformer[ID, BsonValue], catConv: Transformer[CAT, BsonValue]) extends EventStore[ID, EVT, CAT] {
  import MongoEventStore._

  protected[this] implicit val idConvForth = idConv.forth _
  protected[this] implicit val idConvBack = idConv.back _
  protected[this] implicit val catConvForth = catConv.forth _
  protected[this] implicit val catConvBack = catConv.back _

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
    query("stream.id" := stream, OrderByRevision_asc, callback)
  }

  def replayStreamSince[T](stream: ID, sinceRevision: Long)(callback: Iterator[Transaction] ⇒ T): T = {
    val filter = obj(
      "stream" := obj(
        "id" := stream,
        "rev" := $gt(sinceRevision)))
    query(filter, OrderByRevision_asc, callback)
  }
  def replayStreamTo[T](stream: ID, toRevision: Long)(callback: Iterator[Transaction] ⇒ T): T = {
    val filter = obj(
      "stream" := obj(
        "id" := stream,
        "rev" := $lte(toRevision)))
    query(filter, OrderByRevision_asc, callback)
  }
  def replayStreamRange[T](stream: ID, revisionRange: collection.immutable.NumericRange[Long])(callback: Iterator[Transaction] ⇒ T): T = {
    val lowerBound = $gte(revisionRange.head)
    val upperBound = if (revisionRange.isInclusive) $lte("stream.rev" := revisionRange.last) else $lt("stream.rev" := revisionRange.last)
    val filter = obj(
      "stream" := obj(
        "id" := stream,
        "rev" := obj(lowerBound, upperBound)))
    val txnCallback = if (revisionRange.step == 1L) callback else (iter: Iterator[Transaction]) ⇒ callback(iter.filter(txn ⇒ revisionRange.contains(txn.revision)))
    query(filter, OrderByRevision_asc, txnCallback)
  }

  def replay[T](categories: CAT*)(txnHandler: Iterator[Transaction] ⇒ T): T = {
    val filter = categories.length match {
      case 0 ⇒ obj()
      case 1 ⇒ obj("category" := categories.head)
      case _ ⇒ obj("category" := $in(categories: _*))
    }
    query(filter, OrderByTime_asc, txnHandler)
  }

  def replayFrom[T](fromTime: Date, categories: CAT*)(txnHandler: Iterator[Transaction] ⇒ T): T = {
    val filter = obj("time" := $gte(fromTime))
    categories.length match {
      case 0 ⇒ // Ignore
      case 1 ⇒ filter.add("category" := categories.head)
      case _ ⇒ filter.add("category" := $in(categories: _*))
    }
    query(filter, OrderByTime_asc, txnHandler)
  }

  def record(category: CAT, streamId: ID, revision: Long, events: List[_ <: EVT], metadata: Map[String, String]) {
    val timestamp = new scuff.Timestamp
    val doc = obj(
      "time" := timestamp,
      "category" := category,
      "stream" := obj(
        "id" := streamId,
        "rev" := revision),
      "events" := toBsonList(events))
    if (!metadata.isEmpty) doc.add("metadata" := metadata)
    try {
      store.safeInsert(doc)
    } catch {
      case _: MongoException.DuplicateKey ⇒ throw new DuplicateRevisionException
    }
    publish(new Transaction(timestamp, category, streamId, revision, metadata, events))
  }

  private def tryRecord(category: CAT, streamId: ID, revision: Long, events: List[_ <: EVT], metadata: Map[String, String]): Long = try {
    record(category, streamId, revision, events, metadata)
    revision
  } catch {
    case _: DuplicateRevisionException ⇒ tryRecord(category, streamId, revision + 1L, events, metadata)
  }

  def append(category: CAT, streamId: ID, events: List[_ <: EVT], metadata: Map[String, String]): Long = {
    val revision = store.find("stream.id" := streamId).sort(OrderByRevision_desc).limit(1).nextOpt.map(_.apply("stream.rev").as[Long]).getOrElse(-1L) + 1L
    tryRecord(category, streamId, revision, events, metadata)
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
    val timestamp = doc("time").as[Timestamp]
    val category = doc("category").as[CAT]
    val (id, revision) = doc("stream").as[DBObject].map { stream ⇒ (stream("id").as[ID], stream.getAs[Long]("rev")) }
    val metadata = doc("metadata").opt[Map[String, String]].getOrElse(Map.empty)
    val events = doc("events").asSeq[DBObject].map(toEvent)
    new Transaction(timestamp, category, id, revision, metadata, events.toList)
  }

}
