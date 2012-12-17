package scuff.ddd.util

import scuff.ddd._
import scuff.Mongolia._
import com.mongodb._
import org.bson.types._
import scuff.es.util.MongoEventStore

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
abstract class MongoDomainEventStore[ID](collection: DBCollection)(implicit bsonConverter: ID ⇒ BsonValue, idExtractor: BsonValue ⇒ ID) extends MongoEventStore[ID, DomainEvent](collection) {

  protected def snapshotData(evt: DomainEvent): DBObject
  protected def rebuildEvent(name: String, version: Short, data: DBObject): DomainEvent

  final protected def toDBObject(evt: DomainEvent): DBObject = obj(
    "name" := evt.eventName,
    "version" := evt.typeVersion,
    "data" := snapshotData(evt)
  )
  final protected def toEvent(dbo: DBObject): DomainEvent = {
    val name = dbo.getAs[String]("name")
    val version = dbo.getAs[Int]("version").asInstanceOf[Short]
    rebuildEvent(name, version, dbo.getAs[DBObject]("data"))
  }

}
