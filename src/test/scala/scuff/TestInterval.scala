package scuff

import org.junit._
import org.junit.Assert._

class TestInterval {

  @Test
  def parse {
      def assertSome(i: Interval[BigDecimal]) {
        assertFalse(i contains 44)
        assertTrue(i contains 45)
        assertFalse(i contains 59)
        assertFalse(i contains 60)
        assertTrue(i contains 50)
      }
    Seq(
      "[45.00;59.00)", "[45.00;59.00[", "[45.00,59.00)", "[45.00,59.00[",
      "[45,00;59,00)", "[45,00;59,00["
    ).map(s ⇒ s -> Interval.parse(s)).foreach {
        case (str, si) ⇒
          si match {
            case None ⇒ fail("Should be valid interval string: " + str)
            case Some(si) ⇒ assertSome(si)
          }
      }
  }

  @Test
  def range {
      def assertSome(i: Interval[Int]) {
        assertFalse(i contains 44)
        assertTrue(i contains 45)
        assertFalse(i contains 59)
        assertFalse(i contains 60)
        assertTrue(i contains 50)
      }
    assertSome(45 until 59)
    assertSome(45 to 58)
  }

  @Test
  def numrange {
      def assertSome(i: Interval[Long]) {
        assertFalse(i contains 44)
        assertTrue(i contains 45)
        assertFalse(i contains 59)
        assertFalse(i contains 60)
        assertTrue(i contains 50)
      }
    assertSome(45L until 59)
    assertSome(45L to 58)
  }

  @Test
  def tuple {
      def assertSome(i: Interval[Long]) {
        assertFalse(i contains 44)
        assertTrue(i contains 45)
        assertFalse(i contains 59)
        assertFalse(i contains 60)
        assertTrue(i contains 50)
        assertEquals("[45,59)", i.toString)
      }
    assertSome(45L -> 59L)
  }

  @Test
  def `to string` {
    val iStr = "[45,99;59,25["
    val i = Interval.parse(iStr).get
    assertEquals(iStr, i.toString)
    val i2 = Interval(45.99f until 59.25f)
    assertEquals("[45.99,59.25)", i2.toString)
    val i3 = Interval(1f to Float.PositiveInfinity)
    assertEquals("[1.0,∞)", i3.toString)
    val i4 = Interval(Float.NegativeInfinity to 1f)
    assertEquals("(-∞,1.0]", i4.toString)
    val i5 = Interval(Float.NegativeInfinity to Float.PositiveInfinity)
    assertEquals("(-∞,∞)", i5.toString)
    assertEquals("(-∞,∞)", Interval.Unbounded.toString)
  }

  @Test
  def invalid {
    assertEquals(None, Interval.parse("[45,99;59.25["))
    assertEquals(None, Interval.parse("[45.99;59,25["))
    assertEquals(None, Interval.parse("[45.99,59,25["))
    assertEquals(None, Interval.parse("[45,99 59,25["))
    try {
      Interval(Double.NaN -> 5d)
      fail("Should fail on NaN")
    } catch {
      case e: IllegalArgumentException ⇒ assertTrue(e.getMessage.contains("NaN"))
    }
    try {
      Interval(1d -> Double.NaN)
      fail("Should fail on NaN")
    } catch {
      case e: IllegalArgumentException ⇒ assertTrue(e.getMessage.contains("NaN"))
    }
    try {
      Interval(Float.NaN -> 5f)
      fail("Should fail on NaN")
    } catch {
      case e: IllegalArgumentException ⇒ assertTrue(e.getMessage.contains("NaN"))
    }
    try {
      Interval(1f -> Float.NaN)
      fail("Should fail on NaN")
    } catch {
      case e: IllegalArgumentException ⇒ assertTrue(e.getMessage.contains("NaN"))
    }
  }

}