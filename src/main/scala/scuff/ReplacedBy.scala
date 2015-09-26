package scuff

import scala.annotation.StaticAnnotation
import scala.reflect.runtime.universe._
import scala.collection.concurrent.TrieMap
import scala.reflect._
import scala.reflect.api.TypeCreator
import scala.reflect.api.Universe
import scala.annotation.ClassfileAnnotation

/**
  * Trait to provide an upgrade path to versioned types.
  */
trait ReplacedBy[T] {
  def upgrade(): T
}
