package scuff.eventual.mongo

import com.mongodb.DB
import scuff.Mongolia._
import com.mongodb.DBObject
import collection.JavaConverters._
import com.mongodb.{ DB, DBObject }
import redis.clients.jedis.{ JedisPool, JedisShardInfo }
import scuff.Threads.PiggyBack
import scuff.Mongolia.{ enrich, impoverish }
import scuff.redis.{ RedisChannel, RedisConnectionPool, RedisPublisher, threadSafe }
import com.mongodb.WriteConcern
import scuff.Mongolia._
import com.mongodb.DBCollection

private[mongo] object MongoExceptionStore {
  private val StkTrcCdc = new scuff.Codec[StackTraceElement, BsonValue] {
    def encode(ste: StackTraceElement): BsonValue = {
      val doc = obj(ignoreNulls = true).add("class" := ste.getClassName, "method" := ste.getMethodName, "file" := ste.getFileName)
      if (ste.getLineNumber >= 0) doc.add("line" := ste.getLineNumber)
      doc
    }
    def decode(b: BsonValue): StackTraceElement = b.raw match {
      case doc: DBObject ⇒
        new StackTraceElement(doc("class").as[String], doc("method").as[String], doc("file").opt[String].orNull, doc("line").opt[Int].getOrElse(-2))
    }
  }
}

abstract class MongoExceptionStore(dbColl: DBCollection)
    extends MongoEventStore[String, List[StackTraceElement], Class[_]](dbColl)
    with scuff.eventual.util.ExceptionStore {

  import MongoExceptionStore._

  protected def toDBObject(evt: List[StackTraceElement]): com.mongodb.DBObject = arr(evt.map(StkTrcCdc.encode): _*)
  protected def toEvent(dbo: com.mongodb.DBObject): List[StackTraceElement] = dbo match {
    case list: java.lang.Iterable[_] ⇒ list.asScala.map {
      case doc: DBObject ⇒ StkTrcCdc.decode(doc)
    }.toList
  }

}
