package scuff.eventual.mongo

import scuff.ddd._
import scuff.Mongolia._
import com.mongodb._
import org.bson.types._

/**
 * Stores events, using this format:
 * {{{
 *   {
 *     name: "EventName",
 *     version: 4,
 *     data: {}
 *     }
 * }}}
 */
abstract class MongoDomainEventStore[ID, CAT](collection: DBCollection)(implicit idCdc: scuff.Codec[ID, BsonValue], catCdc: scuff.Codec[CAT, BsonValue])
    extends MongoEventStore[ID, DomainEvent, CAT](collection) {

  /** Encode the data part. */
  protected def encodeData(evt: DomainEvent): DBObject
  /** Decode the data part. */
  protected def decodeData(name: String, version: Short, data: DBObject): DomainEvent

  final protected def toDBObject(evt: DomainEvent): DBObject =
    obj(
      "name" := evt.eventName,
      "version" := evt.typeVersion,
      "data" := encodeData(evt))
  final protected def toEvent(dbo: DBObject): DomainEvent = {
    val name = dbo.getAs[String]("name")
    val version = dbo.getAs[Int]("version").asInstanceOf[Short]
    decodeData(name, version, dbo.getAs[DBObject]("data"))
  }

}
