package scuff.ddd.util

import scala.collection.immutable.{ Seq => ISeq }
import scala.collection.concurrent.{ Map => CMap, TrieMap }
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.control.NonFatal
import scuff.ddd._
import scuff.concurrent._

class ConcurrentMapRepository[AR, ID](
  map: CMap[ID, (AR, Int)] = new TrieMap[ID, (AR, Int)])(implicit ec: ExecutionContext = Threads.Blocking)
    extends Repository[AR, ID] {

  def insert(id: ID, ar: AR, metadata: Map[String, String]): Future[Int] = Future {
    map.putIfAbsent(id, ar -> 0) match {
      case None => 0
      case _ => throw new DuplicateIdException(id)
    }
  }

  def exists(id: ID): Future[Option[Int]] = Future(map.get(id).map(_._2))

  def load(id: ID): Future[(AR, Int)] = Future {
    map.get(id) match {
      case None => throw new UnknownIdException(id)
      case Some(found) => found
    }
  }

  private def tryUpdate(id: ID, expectedRevision: Option[Int], metadata: Map[String, String], updateThunk: (AR, Int) => Future[AR]): Future[Int] = {
    map.get(id) match {
      case None => Future failed new UnknownIdException(id)
      case Some(oldAR @ (ar, rev)) =>
        updateThunk(ar, rev).flatMap { ar =>
            val newRev = rev + 1
            if (map.replace(id, oldAR, ar -> newRev)) {
              Future successful newRev
            } else {
              tryUpdate(id, expectedRevision, metadata, updateThunk)
            }
        }
    }
  }

  def update(id: ID, expectedRevision: Option[Int], metadata: Map[String, String])(updateThunk: (AR, Int) => Future[AR]): Future[Int] =
    Future(tryUpdate(id, expectedRevision, metadata, updateThunk)).flatten

}
