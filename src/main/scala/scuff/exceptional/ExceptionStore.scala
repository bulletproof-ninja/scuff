package scuff.exceptional

trait ExceptionStore {
  type ID
  case class ExceptionRef(stackTraceId: ID, message: Option[String])
  def getStackTraceId(t: Throwable): ID
  def saveIncident(exceptionChain: List[ExceptionRef], metadata: Map[String, String])
}

class ExceptionTracker[T](store: ExceptionStore { type ID = T }) {

  def store(t: Throwable, metadata: Map[String, String] = Map.empty): Unit = {
    val exceptionChain = registerChain(t)
    store.saveIncident(exceptionChain, metadata)
  }

  private def registerChain(t: Throwable): List[store.ExceptionRef] = {
    val causeChain = t.getCause match {
      case null ⇒ Nil
      case cause ⇒ registerChain(cause)
    }
    new store.ExceptionRef(store.getStackTraceId(t), Option(t.getMessage)) :: causeChain
  }

}

import com.mongodb._

class MongoExeceptionStore(db: DB) extends ExceptionStore {
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

  def saveIncident(exceptionChain: List[ExceptionRef], metadata: Map[String, String]) = {
    val doc = obj("chain" := exceptionChain)
    if (!metadata.isEmpty) doc.add("metadata" := metadata)
    incidents.insert(doc)
  }

}