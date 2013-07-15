package scuff.exceptional.mongo

import scuff.exceptional.Storage
import com.mongodb._
import scuff.Mongolia._
import org.bson.types._

class MongoStorage(stacktraces: DBCollection, incidents: DBCollection) extends Storage {
  type ID = ObjectId

  incidents.setWriteConcern(WriteConcern.NONE)
  stacktraces.setWriteConcern(WriteConcern.FSYNC_SAFE)
  stacktraces.ensureIndex("exception" := ASC, "stackTrace" := ASC)

  def this(db: DB) = this(db("exception.stacktraces"), db("exception.incidents"))

  private[this] implicit val ste2bson = new scuff.Codec[StackTraceElement, BsonValue] {
    def encode(ste: StackTraceElement): BsonValue = {
    val doc = obj("class" := ste.getClassName, "method" := ste.getMethodName)
    if (ste.getLineNumber >= 0) doc.add("line" := ste.getLineNumber)
    if (ste.getFileName != null) doc.add("file" := ste.getFileName)
      doc
    }
    def decode(b: BsonValue): StackTraceElement = sys.error("Not implemented")
  }

  /**
   * There is a slight race condition at play here,
   * which can lead to duplicate stack traces,
   * but since MongoDB appears to enforce unique indices
   * slightly different than how they are looked up,
   * we cannot eliminate this (at least without spending
   * time to understand this problem better).
   */
  def getStackTraceId(t: Throwable): ID = {
    val doc = obj("exception" := t.getClass.getName, "stackTrace" := t.getStackTrace)
    stacktraces.findOpt(doc, obj("_id" := INCLUDE)) match {
      case Some(doc) ⇒ doc("_id").as[ObjectId]
      case None ⇒
        val id = new ObjectId
        doc.add("_id" := id)
        stacktraces.safeInsert(doc)
        id
    }
  }

  private[this] implicit val eref2bson = new scuff.Codec[ExceptionRef, BsonValue] {
    def encode(eref: ExceptionRef): BsonValue = {
    val doc = obj("stackTrace" := eref.stackTraceId)
    eref.message.foreach(msg ⇒ doc.add("message" := msg))
    doc
  }
    def decode(b: BsonValue): ExceptionRef = sys.error("Not implemented")
  }

  def saveIncident(exceptionChain: List[ExceptionRef], time: Long, metadata: Map[String, String]) = {
    val doc = obj("chain" := exceptionChain, "time" := new java.util.Date(time))
    if (!metadata.isEmpty) doc.add("metadata" := metadata)
    incidents.insert(doc)
  }
}
