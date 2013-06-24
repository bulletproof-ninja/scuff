package scuff

case class GeoPoint(latitude: Float, longitude: Float) {
  GeoPoint.verify("latitude", latitude)
  GeoPoint.verify("longitude", longitude)
}

object GeoPoint {
  import scala.util.Try
  private val regex = """^(-?\d{1,3})(?:[.,·'](\d*))?[^\d-]+(-?\d{1,3})(?:[.,·'](\d*))?$""".r
  private def verify(name: String, latLon: Float) = require(-180f <= latLon && latLon <= 180f, "Invalid " + name + ": " + latLon)
  def parse(str: String): Try[GeoPoint] = Try {
    regex.findFirstMatchIn(str) match {
      case None ⇒ throw new IllegalArgumentException("Cannot parse: \"%s\"".format(str))
      case Some(m) ⇒
        val lat = (m.group(1) + "." + m.group(2)).toFloat
        val lng = (m.group(3) + "." + m.group(4)).toFloat
      new GeoPoint(lat, lng)
    }
  }
}

