package scuff

import org.junit._
import org.junit.Assert._

class TestGeoPoint {
  @Test
  def `dot decimal` {
    assertEquals(new GeoPoint(-23.34534d, 12.54642d), GeoPoint.parse("-23.34534, 12.54642").get)
    assertEquals(new GeoPoint(23.34534d, -12.54642d), GeoPoint.parse("23.34534 -12.54642").get)
    assertEquals(new GeoPoint(-23.34534d, -12.54642d), GeoPoint.parse("-23.34534 -12.54642").get)
    assertEquals(new GeoPoint(85.5464245621354678d, 124.3453454654631798d), GeoPoint.parse("85.5464245621354678 124.3453454654631798").get)
  }

  @Test
  def `comma decimal` {
    assertEquals(new GeoPoint(-23.34534d, 12.54642d), GeoPoint.parse("-23,34534 : 12,54642").get)
    assertEquals(new GeoPoint(23.34534d, -12.54642d), GeoPoint.parse("23,34534 -12,54642").get)
    assertEquals(new GeoPoint(-23.34534d, -12.54642d), GeoPoint.parse("-23,34534 -12,54642").get)
    assertEquals(new GeoPoint(85.5464245621354678d, 124.3453454654631798d), GeoPoint.parse("85,5464245621354678, 124,3453454654631798").get)
  }

  @Test()
  def `out of positive bounds` {
    assertTrue(GeoPoint.parse("-23,34534 : 180,54642").isFailure)
  }

  @Test()
  def `out of negative bounds` {
    assertTrue(GeoPoint.parse("-230.34534 : 179,54642").isFailure)
  }
  @Test
  def `invalid` {
    assertTrue(GeoPoint.parse("23").isFailure)
  }

  @Test
  def distance {
    val p1 = GeoPoint.parse("35.0303, -111.0286").get
    val p2 = GeoPoint.parse("35.0255, -111.0161").get
    assertEquals(1250d, p1.distance(p2), 10)
  }
}
