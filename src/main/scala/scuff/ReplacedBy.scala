package scuff

/**
  * Trait to provide an upgrade path to versioned types.
  */
trait ReplacedBy[T] {
  def upgrade(): T
}
