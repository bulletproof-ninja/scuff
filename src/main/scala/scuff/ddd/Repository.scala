package scuff.ddd

import scala.collection.immutable.{ Seq, Map }
import scala.concurrent.{ ExecutionContext, Future }
import scala.language.implicitConversions
import scala.util.{ Failure, Success, Try }
import scala.util.control.NonFatal
import scuff.concurrent.Threads.PiggyBack
import scuff.concurrent.Threads

/**
  * Entity repository.
  */
trait Repository[ID, T] {

  /**
    * Get current revision, if exists.
    * @return Current revision or `None` if unknown id.
    */
  def exists(id: ID): Future[Option[Int]]

  /**
    * Load entity. Only for reading.
    * Modifications cannot be saved.
    * @param id The instance ID
    * @return The latest revision of entity or [[scuff.ddd.UnknownIdException]]
    */
  def load(id: ID): Future[(T, Int)]
  /**
    * Update entity.
    * @param id The instance id
    * @param revisionToUpdate The revision that is expected to be updated.
    * If this differs from actual current revision, the update can be rejected.
    * Using `None` will ignore any concurrent updates, so last update always wins.
    * @param metadata Optional metadata.
    * @param updateThunk The code block responsible for updating.
    * Will receive the instance and revision.
    * @return New revision, or [[scuff.ddd.UnknownIdException]] if unknown id.
    */
  def update(
    id: ID, revisionToUpdate: Option[Int], metadata: Map[String, String] = Map.empty)(
      updateThunk: (T, Int) => Future[T]): Future[Int]

  /**
    * Update entity.
    * @param id The instance id
    * @param revisionToUpdate The revision that is expected to be updated.
    * If this differs from actual current revision, the update can be rejected.
    * @param metadata Metadata.
    * @param updateThunk The code block responsible for updating.
    * Will receive the instance and current revision.
    * @return New revision, or [[scuff.ddd.UnknownIdException]] if unknown id.
    */
  final def update(
    id: ID, revisionToUpdate: Int, metadata: Map[String, String])(
      updateThunk: (T, Int) => Future[T]): Future[Int] =
    update(id, Some(revisionToUpdate), metadata)(updateThunk)
  /**
    * Update entity.
    * @param id The instance id
    * @param revisionToUpdate The revision that is expected to be updated.
    * If this differs from actual current revision, the update can be rejected.
    * @param updateThunk The code block responsible for updating.
    * Will receive the instance and current revision.
    * @return New revision, or [[scuff.ddd.UnknownIdException]] if unknown id.
    */
  final def update(
    id: ID, revisionToUpdate: Int)(
      updateThunk: (T, Int) => Future[T]): Future[Int] =
    update(id, Some(revisionToUpdate))(updateThunk)

  /**
    * Insert new entity. Must, by definition, be given revision `0`.
    * @param id The instance id
    * @param entity The instance to insert
    * @param metadata Optional metadata.
    * @return The revision (always `0`) if successful,
    * or [[scuff.ddd.DuplicateIdException]] if id already exists
    */
  def insert(id: ID, entity: T, metadata: Map[String, String] = Map.empty): Future[Int]
}

final case class UnknownIdException(id: Any) extends RuntimeException(s"Unknown id: $id")
final case class DuplicateIdException(id: Any) extends RuntimeException(s"Id already exists: $id")
