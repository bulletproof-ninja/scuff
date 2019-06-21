package scuff

import org.junit._
import Assert._

class TestScuffTraversable {

  @Test
  def `last element`(): Unit = {
      def iter = (1 to 1000).iterator
    assertEquals(1000, iter.last)
    assertEquals(Some(1000), iter.lastOption)
    assertEquals(None, Nil.iterator.lastOption)
    try fail(s"Should fail: ${List[Int]().iterator.last}") catch {
      case _: NoSuchElementException => // Expected
    }
  }

  @Test
  def `head element`(): Unit = {
      def iter = (1 to 1000).iterator
    assertEquals(1, iter.head)
    assertEquals(Some(1), iter.headOption)
    assertEquals(None, Nil.iterator.headOption)
    try fail(s"Should fail: ${List[Int]().iterator.last}") catch {
      case _: NoSuchElementException => // Expected
    }
  }

  @Test
  def `levenshtein`(): Unit = {
    val bytes1: Array[Byte] = Array(4, 3, 7, 8)
    val bytes2: Vector[Byte] = Vector(4, 33, 7, 8)
    assertEquals(1, bytes1 levenshtein bytes2)
    assertEquals(1, bytes2 levenshtein bytes1)
    assertEquals(0, bytes1 levenshtein bytes1)
    assertEquals(0, bytes2 levenshtein bytes2)
  }

  @Test
  def `sub type collection`(): Unit = {
    val numbers: List[AnyVal] = 123 :: 467L :: 435d :: 998f :: Nil
    assertEquals(List(123), numbers.collectAs[Int])
    assertEquals(List(467L), numbers.collectAs[Long])
    assertEquals(List(435d), numbers.collectAs[Double])
    assertEquals(List(998f), numbers.collectAs[Float])
    assertEquals(numbers, numbers.collectAs[AnyVal])
  }
}
