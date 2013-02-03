package scuff.es.util

import com.mongodb._
import scuff.Mongolia._

/**
  * Keep track of handled [[Transaction]]s, so process can resume
  * after shutdown.
  */
abstract class MongoEventConsumer[ID <% BsonValue, EVT](
    dbColl: DBCollection,
    txnDocID: BsonValue) extends scuff.es.PersistentEventConsumer[ID, EVT] {

  protected def writeConcern: WriteConcern = WriteConcern.UNACKNOWLEDGED

  private[this] val syncLock = new AnyRef

  private[this] val txnDoc = {
    val doc = obj("_id" := txnDocID)
    dbColl.findOpt(doc).getOrElse(doc)
  }
  private[this] val revDocSave = obj()
  private[this] val revDocQry = obj()

  final def lastProcessedTxn: Option[BigInt] = syncLock.synchronized {
    txnDoc("last").opt[Array[Byte]].map(BigInt(_))
  }

  final def lastProcessedRev(streamId: ID): Option[Long] = syncLock.synchronized {
    revDocQry += ("_id" := streamId)
    dbColl.findOne(revDocQry) match {
      case null ⇒ None
      case doc ⇒ Some(doc.getAs[Long]("rev"))
    }
  }

  protected def processNext(txn: TXN)

  final def consume(txn: TXN) = syncLock.synchronized {
    processNext(txn)
    revDocSave.add("_id" := txn.streamId, "rev" := txn.revision)
    dbColl.save(revDocSave, writeConcern)
    txnDoc.put("last", txn.transactionID.toByteArray)
    dbColl.save(txnDoc, writeConcern)
  }

}
