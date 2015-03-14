package scuff

import org.junit._
import Assert._

class TestScuffMap {
  @Test
  def merge {
    val m1 = Map("a" -> 4, "b" -> 6, "c" -> 2)
    val m2 = Map("b" -> 7, "c" -> 3, "d" -> 7)
    val m3 = m1.merge(m2, (i1, i2) => i1 + i2)
    assertEquals(4, m3("a"))
    assertEquals(13, m3("b"))
    assertEquals(5, m3("c"))
    assertEquals(7, m3("d"))
  }
}