package scuff.geo

/**
 * WGS-84 geographical circle.
 * @param center The center of circle
 * @param radius The radius from center, in meters
 */
case class Circle(center: Point, radius: Float) {
  require(radius >= 0f, "Radius cannot be negative or NaN: " + radius)

  def this(latitude: Double, longitude: Double, radius: Float) =
    this(new Point(latitude, longitude), radius)

  def contains(p: Point): Boolean = center.distance(p) <= radius
}
