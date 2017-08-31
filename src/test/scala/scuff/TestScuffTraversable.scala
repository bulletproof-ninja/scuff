package scuff

import org.junit._
import Assert._

class TestScuffTraversable {
  @Test
  def `optional list` {
    assertEquals(None, Nil.optional)
    assertEquals(None, List.empty[Int].optional)
    assertEquals(Some(List(1)), List(1).optional)
    assertEquals(Some(List(1, 2, 3)), List(1, 2, 3).optional)
  }

  @Test
  def `last element` {
      def iter = (1 to 1000).iterator
    assertEquals(1000, iter.last)
    assertEquals(Some(1000), iter.lastOption)
    assertEquals(None, Nil.iterator.lastOption)
    try fail(s"Should fail: ${List[Int]().iterator.last}") catch {
      case _: NoSuchElementException => // Expected
    }
  }

  @Test
  def `head element` {
      def iter = (1 to 1000).iterator
    assertEquals(1, iter.head)
    assertEquals(Some(1), iter.headOption)
    assertEquals(None, Nil.iterator.headOption)
    try fail(s"Should fail: ${List[Int]().iterator.last}") catch {
      case _: NoSuchElementException => // Expected
    }
  }

}
