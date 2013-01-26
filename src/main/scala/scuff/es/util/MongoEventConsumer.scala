package scuff.es.util

import com.mongodb._
import scuff.Mongolia._

/**
  * Keep track of handled [[Transaction]]s, so process can resume
  * after shutdown.
  */
abstract class MongoEventConsumer[ID, EVT](
    coll: DBCollection,
    txnDocID: BsonValue) extends scuff.es.PersistentEventConsumer[ID, EVT] {

  private[this] val dbColl = enrich(coll)

  private[this] val txnDoc = {
    val doc = obj("_id" := txnDocID)
    dbColl.findOpt(doc).getOrElse(doc)
  }

  final def lastProcessedTxn: Option[BigInt] = txnDoc.synchronized {
    txnDoc("last").opt[Array[Byte]].map(BigInt(_))
  }

  protected def next(txn: TXN)

  final def consume(txn: TXN) = txnDoc.synchronized {
    next(txn)
    txnDoc.put("last", txn.transactionID.toByteArray)
    dbColl.save(txnDoc)
  }

}
