package scuff

/**
 * Trait to provide versioning to classes.
 */
trait TypeVersioned {
  def typeVersion: Short
}

/**
 * Trait to provide an upgrade path to versioned classes.
 */
trait ReplacedBy[T <: TypeVersioned] extends TypeVersioned {
  def upgrade(): T
}
