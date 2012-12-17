package scuff

case class GeoPoint(latitude: Float, longitude: Float) {
  GeoPoint.verify("latitude", latitude)
  GeoPoint.verify("longitude", longitude)
}

object GeoPoint {
  private val regex = """((-?\d{1,3})\D(\d+))[^\d-]+((-?\d{1,3})\D(\d+))""".r
  private def verify(name: String, latLon: Float) = require(-180f <= latLon && latLon <= 180f, "Invalid " + name + ": " + latLon)
  def parse(str: String) = try {
    regex.findFirstMatchIn(str).map { m ⇒
      val lat = (m.group(2) + "." + m.group(3)).toFloat
      val lng = (m.group(5) + "." + m.group(6)).toFloat
      new GeoPoint(lat, lng)
    }
  } catch {
    case e: Exception ⇒ None
  }
}

