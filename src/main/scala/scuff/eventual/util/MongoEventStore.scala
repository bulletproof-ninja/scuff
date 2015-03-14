package scuff.eventual.util

import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success
import com.mongodb.{DB, DBCollection, DBObject, DuplicateKeyException, WriteConcern}
import scuff.{Timestamp, eventual}
import scuff.Codec
import scuff.Mongolia._
import scuff.Threads.PiggyBack
import scala.util.Failure

object MongoEventStore {
  private final val OrderByTime_asc = obj("time" := ASC, "_id.stream" := ASC, "_id.rev" := ASC)
  private final val OrderByRevision_asc = obj("_id.rev" := ASC)

  private def ensureIndicies(coll: DocCollection): DocCollection = {
    coll.createIndex("_id.stream" := ASC, "_id.rev" := ASC)
    coll.createIndex("time")
    coll.createIndex("category")
    coll
  }

  def getCollection(name: String, db: DB): DBCollection = {
    val wc = if (db.getMongo.getReplicaSetStatus == null) WriteConcern.FSYNCED else WriteConcern.MAJORITY
    db(name, wc)
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
    extends eventual.EventStore[ID, EVT, CAT] { expectedTrait: EventStorePublisher[ID, EVT, CAT] =>

  import MongoEventStore._

  protected implicit def mongoExecCtx: ExecutionContext

  private[this] val store = ensureIndicies(dbColl)

  protected def toDBObject(evt: EVT): DBObject
  protected def toEvent(dbo: DBObject): EVT

  @annotation.tailrec
  private def toBsonList(events: List[_ <: EVT], list: BsonList = new BsonList): BsonList = events match {
    case Nil => list
    case head :: tail =>
      list += toDBObject(head)
      toBsonList(tail, list)
  }

  def exists(stream: ID): Future[Boolean] = Future {
    store.find(obj("_id.stream" := stream)).limit(1).size() != 0
  }

  def replayStream[T](stream: ID)(callback: Iterator[Transaction] => T): Future[T] = {
    query(obj("_id.stream" := stream), OrderByRevision_asc, callback)
  }

  def replayStreamAfter[T](stream: ID, afterRevision: Int)(callback: Iterator[Transaction] => T): Future[T] = {
    val filter = obj(
      "_id.stream" := stream,
      "_id.rev" := $gt(afterRevision))
    query(filter, OrderByRevision_asc, callback)
  }
  def replayStreamTo[T](stream: ID, toRevision: Int)(callback: Iterator[Transaction] => T): Future[T] = {
    val filter = obj(
      "_id.stream" := stream,
      "_id.rev" := $lte(toRevision))
    query(filter, OrderByRevision_asc, callback)
  }
  def replayStreamRange[T](stream: ID, revisionRange: collection.immutable.Range)(callback: Iterator[Transaction] => T): Future[T] = {
    require(revisionRange.step == 1, s"Revision range must step by 1 only, not ${revisionRange.step}")
    val filter = obj(
      "_id.stream" := stream,
      "_id.rev" := obj(
        $gte(revisionRange.head),
        $lte(revisionRange.last)))
    query(filter, OrderByRevision_asc, callback)
  }

  def replay[T](categories: CAT*)(txnHandler: Iterator[Transaction] => T): Future[T] = {
    val filter = categories.length match {
      case 0 => obj()
      case 1 => obj("category" := categories.head)
      case _ => obj("category" := $in(categories: _*))
    }
    query(filter, OrderByTime_asc, txnHandler)
  }

  def replayFrom[T](fromTimestamp: Long, categories: CAT*)(txnHandler: Iterator[Transaction] => T): Future[T] = {
    val filter = obj("time" := $gte(fromTimestamp))
    categories.length match {
      case 0 => // Ignore
      case 1 => filter.add("category" := categories.head)
      case _ => filter.add("category" := $in(categories: _*))
    }
    query(filter, OrderByTime_asc, txnHandler)
  }

  // TODO: Make non-blocking once supported by the driver.
  def record(timestamp: Long, category: CAT, stream: ID, revision: Int, events: List[_ <: EVT], metadata: Map[String, String]): Future[Transaction] = {
    val _id = obj("stream" := stream, "rev" := revision)
    val insert = Future {
      val doc = obj(
        "_id" := _id,
        "time" := timestamp,
        "category" := category,
        "events" := toBsonList(events))
      if (!metadata.isEmpty) doc.add("metadata" := metadata)
      store.safeInsert(doc)
      new Transaction(timestamp, category, stream, revision, metadata, events)
    }
    insert.andThen {
      case Success(txn) =>
        try publish(txn) catch {
          case e: Exception => mongoExecCtx.reportFailure(e)
        }
    }
    insert.recover {
      case _: DuplicateKeyException =>
        val conflicting = toTransaction(store.findOne(_id))
        throw new DuplicateRevisionException(stream, conflicting)
    }(PiggyBack)
  }

//  private def tryAppend(category: CAT, stream: ID, revision: Int, events: List[_ <: EVT], metadata: Map[String, String]): Future[Transaction] =
//    record(category, stream, revision, events, metadata).recoverWith {
//      case _: DuplicateRevisionException => tryAppend(category, stream, revision + 1, events, metadata)
//    }
//
//  def append(category: CAT, stream: ID, events: List[_ <: EVT], metadata: Map[String, String]): Future[Transaction] = {
//    val revision = store.find("_id.stream" := stream).last("_id.rev").map(_.getAs[Int]("_id.rev")).getOrElse(-1) + 1
//    tryAppend(category, stream, revision, events, metadata)
//  }

  // TODO: Make non-blocking once supported by the driver.
  private def query[T](filter: DBObject, ordering: DBObject, handler: Iterator[Transaction] => T): Future[T] = Future {
    import collection.JavaConverters._
    val cursor = store.find(filter).sort(ordering)
    try {
      val iterator = (cursor: java.util.Iterator[DBObject]).asScala.map(toTransaction(_))
      handler(iterator)
    } finally {
      cursor.close()
    }
  }

  private def toTransaction(doc: BsonObject): Transaction = {
    val timestamp = doc("time").as[Long]
    val category = doc("category").as[CAT]
    val (id, revision) = doc("_id").as[DBObject].map { id => (id("stream").as[ID], id.getAs[Int]("rev")) }
    val metadata = doc("metadata").opt[Map[String, String]].getOrElse(Map.empty)
    val events = doc("events").asSeq[DBObject].map(toEvent)
    new Transaction(timestamp, category, id, revision, metadata, events.toList)
  }

}

