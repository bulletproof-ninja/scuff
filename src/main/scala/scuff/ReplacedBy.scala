package scuff

/**
  * Trait to provide an upgrade path to versioned types.
  */
trait ReplacedBy[T] {
  def upgrade(): T
}

/**
  * Trait to provide a downgrade path to versioned types.
  */
trait Replaces[T] {
  def downgrade(): T
}
