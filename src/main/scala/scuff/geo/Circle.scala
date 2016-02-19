package scuff.geo

import scala.math.atan2
import scala.math.cos
import scala.math.pow
import scala.math.sin
import scala.math.sqrt
import scala.math.toRadians
import scala.util.Try

/**
 * WGS-84 geographical circle.
 * @param center The center of circle
 * @param radius The radius from center
 */
case class Circle(center: Point, radius: Float) {
  require(radius >= 0f, "Radius cannot be negative or NaN: " + radius)
}
