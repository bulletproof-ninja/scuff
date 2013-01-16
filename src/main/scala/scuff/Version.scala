package scuff

/**
  * Trait to provide versioning to classes.
  */
trait Version {
  def typeVersion: Short
}

/**
  * Trait to provide an upgrade path to versioned classes.
  */
trait ReplacedBy[T <: Version] extends Version {
  def upgrade(): T
}
