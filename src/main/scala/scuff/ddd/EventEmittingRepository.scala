package scuff.ddd

import scala.collection.immutable.Seq
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.control.NonFatal
import scuff.concurrent.Threads

abstract class EventEmittingRepository[ID, T <: AnyRef, EVT](
  val impl: Repository[ID, T], publishCtx: ExecutionContext)
    extends Repository[ID, (T, Seq[EVT])] {

  final type RepoType = (T, Seq[EVT])

  protected def publish(id: ID, revision: Int, events: Seq[EVT], metadata: Map[String, String])

  private def publishEvents(id: ID, revision: Int, events: Seq[EVT], metadata: Map[String, String]) {
    if (events.nonEmpty) try publish(id, revision, events, metadata) catch {
      case NonFatal(e) => publishCtx.reportFailure(e)
    }
  }

  def exists(id: ID): Future[Option[Int]] = impl.exists(id)
  def load(id: ID): Future[(RepoType, Int)] = impl.load(id).map {
    case (entity, rev) => entity -> Nil -> rev
  }(Threads.PiggyBack)
  def update(id: ID, expectedRevision: Option[Int], metadata: Map[String, String])(
    updateThunk: (RepoType, Int) => Future[RepoType]): Future[Int] = {
    @volatile var updatedE: Future[RepoType] = null
    val updatedRev = impl.update(id, expectedRevision, metadata) {
      case (entity, rev) =>
        updatedE = updateThunk(entity -> Nil, rev)
        updatedE.map(_._1)(Threads.PiggyBack)
    }
      implicit def pubCtx = publishCtx
    for (rev <- updatedRev; (entity, events) <- updatedE) {
      publishEvents(id, rev, events, metadata)
    }
    updatedRev
  }

  def insert(id: ID, content: RepoType, metadata: Map[String, String]): Future[Int] = {
    val (state, events) = content
    val inserted = impl.insert(id, state, metadata)
    inserted.foreach { rev =>
      publishEvents(id, rev, events, metadata)
    }(publishCtx)
    inserted
  }

}
