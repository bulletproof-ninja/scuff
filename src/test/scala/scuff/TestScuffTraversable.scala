package scuff

import org.junit._
import Assert._

class TestScuffTraversable {
  @Test
  def `optional list` {
    assertEquals(None, Nil.optional)
    assertEquals(Some(List(1)), List(1).optional)
    assertEquals(Some(List(1,2,3)), List(1,2,3).optional)
  }
}
