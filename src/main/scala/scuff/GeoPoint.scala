package scuff

/**
 * Geographical 2D point.
 * @param latitude The decimal latitude
 * @param longitude The decimal longitude
 * @param radius The radius in meters. Cannot be negative or NaN. Defaults to 0.
 */
case class GeoPoint(latitude: Float, longitude: Float, radius: Float = 0f) {
  require(radius >= 0f, "Radius cannot be negative or NaN: " + radius)
  GeoPoint.verify("latitude", latitude)
  GeoPoint.verify("longitude", longitude)

  private val R = {
    import math._
    // WGS-84 radius
    val a = 6378137.0 // equatorial
    val b = 6356752.314245 // polar
    sqrt(
      (pow(a * a * cos(latitude), 2) + pow(b * b * sin(latitude), 2)) /
        (pow(a * cos(latitude), 2) + pow(b * sin(latitude), 2))
    )
  }

  /** Distance in meters. */
  def distance(that: GeoPoint): Float = {
    import math._
    val R = (this.R + that.R) / 2d
    val dLat = toRadians(this.latitude - that.latitude)
    val dLng = toRadians(this.longitude - that.longitude)
    val a =
      sin(dLat / 2) * sin(dLat / 2) +
        sin(dLng / 2) * sin(dLng / 2) * cos(toRadians(this.latitude)) * cos(toRadians(that.latitude))
    val c = 2 * atan2(sqrt(a), sqrt(1 - a))
    (R * c).asInstanceOf[Float]
  }
}

object GeoPoint {
  import scala.util.Try
  private val regex = """^(-?\d{1,3})(?:[.,·'](\d*))?[^\d-]+(-?\d{1,3})(?:[.,·'](\d*))?$""".r
  private def verify(name: String, latLon: Float) = require(-180f <= latLon && latLon <= 180f, "%s must be within ±180 degrees: %f".format(name, latLon))
  /**
   * Parse string of decimal latitude then longitude, e.g.
   * "35.027311, -111.023075"
   * @param str The lat/lng decimal string
   * @param radius The radius in meters. Cannot be negative or NaN. Defaults to 0.
   */
  def parse(str: String, radius: Float = 0f): Try[GeoPoint] = Try {
    regex.findFirstMatchIn(str) match {
      case None ⇒ throw new IllegalArgumentException("Cannot parse: \"%s\"".format(str))
      case Some(m) ⇒
        val lat = (m.group(1) + "." + m.group(2)).toFloat
        val lng = (m.group(3) + "." + m.group(4)).toFloat
        new GeoPoint(lat, lng, radius)
    }
  }
}
