package scuff.eventual.mongo

import scuff._
import Mongolia._
import com.mongodb._
import org.bson.types._
import java.util.Date
import concurrent._
import scala.util._

private object MongoEventStore {
  final val OrderByTime_asc = obj("time" := ASC)
  final val OrderByRevision_asc = obj("_id.rev" := ASC)

  def ensureIndicies(coll: RichDBCollection): RichDBCollection = {
    coll.ensureIndex("_id.stream" := ASC, "_id.rev" := ASC)
    coll.ensureIndex("time")
    coll.ensureIndex("category")
    coll
  }

}

/**
 * Stores events, using this format:
 * {{{
 *   {
 *     _id: { // Indexed
 *       stream: 34534, // Stream identifier
 *       rev: 987, // Stream revision
 *     }
 *     time: { "$date" : "2011-12-08T18:41:32.079Z"}, // Indexed. For reference purposes.
 *     category: "FooBar",
 *     events: []
 *   }
 * }}}
 */
abstract class MongoEventStore[ID, EVT, CAT](dbColl: DBCollection)(implicit idConv: Codec[ID, BsonValue], catConv: Codec[CAT, BsonValue])
    extends eventual.EventStore[ID, EVT, CAT] {

  import MongoEventStore._

  protected implicit def mongoExecCtx: ExecutionContext

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

  def exists(stream: ID): Future[Boolean] = Future {
    store.find(obj("_id.stream" := stream)).limit(1).size() != 0
  }

  def replayStream[T](stream: ID)(callback: Iterator[Transaction] ⇒ T): Future[T] = {
    query(obj("_id.stream" := stream), OrderByRevision_asc, callback)
  }

  def replayStreamSince[T](stream: ID, sinceRevision: Int)(callback: Iterator[Transaction] ⇒ T): Future[T] = {
    val filter = obj(
      "_id" := obj(
        "stream" := stream,
        "rev" := $gt(sinceRevision)))
    query(filter, OrderByRevision_asc, callback)
  }
  def replayStreamTo[T](stream: ID, toRevision: Int)(callback: Iterator[Transaction] ⇒ T): Future[T] = {
    val filter = obj(
      "_id" := obj(
        "stream" := stream,
        "rev" := $lte(toRevision)))
    query(filter, OrderByRevision_asc, callback)
  }
  def replayStreamRange[T](stream: ID, revisionRange: collection.immutable.Range)(callback: Iterator[Transaction] ⇒ T): Future[T] = {
    val lowerBound = $gte(revisionRange.head)
    val upperBound = if (revisionRange.isInclusive) $lte("_id.rev" := revisionRange.last) else $lt("_id.rev" := revisionRange.last)
    val filter = obj(
      "_id" := obj(
        "stream" := stream,
        "rev" := obj(lowerBound, upperBound)))
    val txnCallback = if (revisionRange.step == 1L) callback else (iter: Iterator[Transaction]) ⇒ callback(iter.filter(txn ⇒ revisionRange.contains(txn.revision)))
    query(filter, OrderByRevision_asc, txnCallback)
  }

  def replay[T](categories: CAT*)(txnHandler: Iterator[Transaction] ⇒ T): Future[T] = {
    val filter = categories.length match {
      case 0 ⇒ obj()
      case 1 ⇒ obj("category" := categories.head)
      case _ ⇒ obj("category" := $in(categories: _*))
    }
    query(filter, OrderByTime_asc, txnHandler)
  }

  def replayFrom[T](fromTime: Date, categories: CAT*)(txnHandler: Iterator[Transaction] ⇒ T): Future[T] = {
    val filter = obj("time" := $gte(fromTime))
    categories.length match {
      case 0 ⇒ // Ignore
      case 1 ⇒ filter.add("category" := categories.head)
      case _ ⇒ filter.add("category" := $in(categories: _*))
    }
    query(filter, OrderByTime_asc, txnHandler)
  }

  // TODO: Make non-blocking once supported by the driver.
  def record(category: CAT, stream: ID, revision: Int, events: List[_ <: EVT], metadata: Map[String, String]): Future[Transaction] = Future {
    val timestamp = new Timestamp
    val doc = obj(
      "_id" := obj(
        "stream" := stream,
        "rev" := revision),
      "time" := timestamp,
      "category" := category,
      "events" := toBsonList(events))
    if (!metadata.isEmpty) doc.add("metadata" := metadata)
    try {
      store.safeInsert(doc)
    } catch {
      case _: MongoException.DuplicateKey ⇒ throw new eventual.DuplicateRevisionException(stream, revision)
    }
    new Transaction(timestamp.asMillis, category, stream, revision, metadata, events)
  }.andThen {
    case Success(txn) ⇒ publish(txn)
  }

  private def tryAppend(category: CAT, stream: ID, revision: Int, events: List[_ <: EVT], metadata: Map[String, String]): Future[Transaction] =
    record(category, stream, revision, events, metadata).recoverWith {
      case _: eventual.DuplicateRevisionException ⇒ tryAppend(category, stream, revision + 1, events, metadata)
    }

  def append(category: CAT, stream: ID, events: List[_ <: EVT], metadata: Map[String, String]): Future[Transaction] = {
    val revision = store.find("_id.stream" := stream).last("_id.rev").map(_.getAs[Int]("_id.rev")).getOrElse(-1) + 1
    tryAppend(category, stream, revision, events, metadata)
  }

  // TODO: Make non-blocking once supported by the driver.
  private def query[T](filter: DBObject, ordering: DBObject, handler: Iterator[Transaction] ⇒ T): Future[T] = Future {
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
    val timestamp = doc("time").as[Long]
    val category = doc("category").as[CAT]
    val (id, revision) = doc("_id").as[DBObject].map { id ⇒ (id("stream").as[ID], id.getAs[Int]("rev")) }
    val metadata = doc("metadata").opt[Map[String, String]].getOrElse(Map.empty)
    val events = doc("events").asSeq[DBObject].map(toEvent)
    new Transaction(timestamp, category, id, revision, metadata, events.toList)
  }

}
