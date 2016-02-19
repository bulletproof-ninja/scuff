package scuff

import org.junit._
import org.junit.Assert._
import scuff.geo._

class TestGeoPoint {
  @Test
  def `dot decimal` {
    assertEquals(new Point(-23.34534d, 12.54642d), Point.parse("-23.34534, +12.54642").get)
    assertEquals(new Point(23.34534d, -12.54642d), Point.parse("23.34534 -12.54642").get)
    assertEquals(new Point(-23.34534d, -12.54642d), Point.parse("-23.34534 -12.54642").get)
    assertEquals(new Point(85.5464245621354678d, 124.3453454654631798d), Point.parse("85.5464245621354678 124.3453454654631798").get)
  }

  @Test
  def `comma decimal` {
    assertEquals(new Point(-23.34534d, 12.54642d), Point.parse("-23,34534 : 12,54642").get)
    assertEquals(new Point(23.34534d, -12.54642d), Point.parse("+23,34534 -12,54642").get)
    assertEquals(new Point(-23.34534d, -12.54642d), Point.parse("-23,34534 -12,54642").get)
    assertEquals(new Point(85.5464245621354678d, 124.3453454654631798d), Point.parse("85,5464245621354678, 124,3453454654631798").get)
  }

  @Test()
  def `out of positive bounds` {
    assertTrue(Point.parse("-23,34534 : 180,54642").isFailure)
  }

  @Test()
  def `out of negative bounds` {
    assertTrue(Point.parse("-230.34534 : 179,54642").isFailure)
  }
  @Test
  def `invalid` {
    assertTrue(Point.parse("23").isFailure)
  }

  @Test
  def distance {
    val p1 = Point.parse("35.0303, -111.0286").get
    val p2 = Point.parse("35.0255, -111.0161").get
    assertEquals(1250d, p1.distance(p2), 10)
  }
}
