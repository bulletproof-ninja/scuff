package scuff

/**
 * Geographical 2D point.
 * @param latitude The decimal latitude
 * @param longitude The decimal longitude
 * @param radius The radius. Cannot be negative or NaN. Defaults to 0.
 */
case class GeoPoint(latitude: Double, longitude: Double, radius: Float = 0f) {
  require(radius >= 0f, "Radius cannot be negative or NaN: " + radius)
  require(-90d <= latitude && latitude <= 90d, s"Latitude must be within ±90 degrees: $latitude")
  require(-180d <= longitude && longitude <= 180d, s"Longitude must be within ±180 degrees: $longitude")

  private lazy val R = {
    import math._
    // WGS-84 radius
    val a = 6378137.0 // equatorial
    val b = 6356752.314245 // polar
    sqrt(
      (pow(a * a * cos(latitude), 2) + pow(b * b * sin(latitude), 2)) /
        (pow(a * cos(latitude), 2) + pow(b * sin(latitude), 2)))
  }

  /** Distance in meters. */
  def distance(that: GeoPoint): Double = {
    import math._
    val R = (this.R + that.R) / 2d
    val dLat = toRadians(this.latitude - that.latitude)
    val dLng = toRadians(this.longitude - that.longitude)
    val a =
      sin(dLat / 2) * sin(dLat / 2) +
        sin(dLng / 2) * sin(dLng / 2) * cos(toRadians(this.latitude)) * cos(toRadians(that.latitude))
    val c = 2 * atan2(sqrt(a), sqrt(1 - a))
    (R * c)
  }
}

object GeoPoint {
  import scala.util.Try
  private val regex = """^(-?\d{1,3})(?:[.,·'](\d*))?[^\d-]+(-?\d{1,3})(?:[.,·'](\d*))?$""".r

  /**
   * Parse string of decimal latitude then longitude, e.g.
   * "35.027311, -111.023075"
   * @param str The lat/lng decimal string
   * @param radius The radius in meters. Cannot be negative or NaN. Defaults to 0.
   */
  def parse(str: String, radius: Float = 0f): Try[GeoPoint] = Try {
    regex.findFirstMatchIn(str) match {
      case None => throw new IllegalArgumentException("Cannot parse: \"%s\"".format(str))
      case Some(m) =>
        val lat = (m.group(1) + "." + m.group(2)).toFloat
        val lng = (m.group(3) + "." + m.group(4)).toFloat
        new GeoPoint(lat, lng, radius)
    }
  }
}
