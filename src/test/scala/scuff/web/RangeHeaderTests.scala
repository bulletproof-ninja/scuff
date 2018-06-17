package scuff.web

import org.junit._, Assert._

class RangeHeaderTests {

  @Test
  def simple(): Unit = {
    val Some(header) = RangeHeader("bytes=0-99")
    val Seq(r1) = header.ranges
    assertEquals("bytes", header.unit)
    assertEquals(Some(100L), r1.length)
    assertEquals(0L, r1.start)
    assertEquals(Some(99L), r1.end)
  }

  @Test
  def `assuming a representation of length 10000, the final 500 bytes (byte offsets 9500-9999, inclusive)`(): Unit = {

    val Some(h1) = RangeHeader("bytes=-500")
    val Seq(r1) = h1.ranges
    assertEquals("bytes", h1.unit)
    assertEquals(-500L, r1.start)
    assertEquals(None, r1.end)

    val cr1a = r1.contentRange(10000)
    assertEquals("bytes", cr1a.unit)
    assertEquals(9500L, cr1a.range.get.start)
    assertEquals(9999L, cr1a.range.get.end)
    assertEquals(Some(10000L), cr1a.size)

    val cr1b = r1.contentRange()
    assertEquals("bytes", cr1b.unit)
    assertEquals(None, cr1b.range)
    assertEquals(None, cr1b.size)

    val Some(h2) = RangeHeader("bytes=9500-")
    val Seq(r2) = h2.ranges
    assertEquals("bytes", h2.unit)
    assertEquals(9500L, r2.start)
    assertEquals(None, r2.end)

    val cr2a = r2.contentRange(10000)
    assertEquals("bytes", cr2a.unit)
    assertEquals(9500L, cr2a.range.get.start)
    assertEquals(9999L, cr2a.range.get.end)
    assertEquals(Some(10000L), cr2a.size)

    val cr2b = r2.contentRange()
    assertEquals("bytes", cr2b.unit)
    assertEquals(None, cr2b.range)
    assertEquals(None, cr2b.size)

  }

  @Test
  def `The first and last bytes only (bytes 0 and 9999)`(): Unit = {
    val Some(header) = RangeHeader("bytes=0-0,-1")
    val Seq(r1, r2) = header.ranges
    assertEquals(Some(1L), r1.length)
    assertEquals(None, r2.length)
    assertEquals(1L, r2.lengthGivenSize(10000))
  }

}
