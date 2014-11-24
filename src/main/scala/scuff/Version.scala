package scuff

/**
  * Trait to provide versioning to types.
  */
trait Version {
  def typeVersion: Short
}

/**
  * Trait to provide an upgrade path to versioned types.
  */
trait ReplacedBy[T <: Version] extends Version {
  def upgrade(): T
}
