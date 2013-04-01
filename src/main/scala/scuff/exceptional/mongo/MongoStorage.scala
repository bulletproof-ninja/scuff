package scuff.exceptional.mongo

import scuff.exceptional.Storage
import com.mongodb._

class MongoStorage(db: DB) extends Storage {
  import scuff.Mongolia._
  import org.bson.types._
  type ID = ObjectId

  private[this] val stacktraces = {
    val c = db("stacktraces")
    c.setWriteConcern(WriteConcern.FSYNC_SAFE)
    c.ensureUniqueIndex("exception" := ASC, "stackTrace" := ASC)
    c
  }
  private[this] val incidents = {
    val c = db("incidents")
    c.setWriteConcern(WriteConcern.NONE)
    c
  }

  private[this] implicit val ste2bson = (ste: StackTraceElement) ⇒ {
    val doc = obj("class" := ste.getClassName, "method" := ste.getMethodName)
    if (ste.getLineNumber > 0) doc.add("line" := ste.getLineNumber)
    if (ste.getFileName != null) doc.add("file" := ste.getFileName)
    doc: BsonValue
  }

  def getStackTraceId(t: Throwable): ID = {
    val doc = obj("exception" := t.getClass.getName, "stackTrace" := t.getStackTrace)
    stacktraces.findOpt(doc, obj("_id" := INCLUDE)) match {
      case Some(doc) ⇒ doc("_id").as[ObjectId]
      case None ⇒
        val id = new ObjectId
        doc.add("_id" := id)
        try {
          stacktraces.insert(doc)
          id
        } catch {
          case _: MongoException.DuplicateKey ⇒ getStackTraceId(t)
        }
    }
  }

  private[this] implicit val eref2bson = (eref: ExceptionRef) ⇒ {
    val doc = obj("stackTrace" := eref.stackTraceId)
    eref.message.foreach(msg ⇒ doc.add("message" := msg))
    doc
  }

  def saveIncident(exceptionChain: List[ExceptionRef], time: Long, metadata: Map[String, String]) = {
    val doc = obj("chain" := exceptionChain, "time" := new java.util.Date(time))
    if (!metadata.isEmpty) doc.add("metadata" := metadata)
    incidents.insert(doc)
  }

}