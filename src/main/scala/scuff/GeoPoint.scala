package scuff

/**
 * Geographical point.
 * @param latitude The decimal latitude
 * @param longitude The decimal longitude
 * @param radius The radius in meters. Cannot be negative or NaN. Defaults to 0.
 */
case class GeoPoint(latitude: Float, longitude: Float, radius: Float = 0f) {
  require(radius >= 0f, "Radius cannot be negative or NaN: " + radius)
  GeoPoint.verify("latitude", latitude)
  GeoPoint.verify("longitude", longitude)
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
