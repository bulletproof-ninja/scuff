package scuff

import org.junit._, Assert._
import scala.collection.immutable.HashMap

class TestMonoid {
  @Test
  def sum(): Unit = {
    implicit val ms = Monoid.Sum[Int]
    val list: List[Int] = 51 :: 17 :: 99 :: Nil
    val sum = ms reduce list
    assertEquals(51 + 17 + 99, sum)
  }
  @Test
  def product(): Unit = {
    val m = Monoid.Product[Int]
    val list: List[Int] = 51 :: 17 :: 99 :: Nil
    val prod = list.foldLeft(m.identity)(m.op)
    assertEquals(51 * 17 * 99, prod)
  }
  @Test
  def `Sum HashMap values`(): Unit = {
    val hm1 = HashMap[String, Long]("a" -> 22, "b" -> 77, "c" -> 111)
    val hm2 = HashMap[String, Long]("c" -> 9, "d" -> 5, "e" -> 0)
    val expected = HashMap[String, Long]("a" -> 22, "b" -> 77, "c" -> (111 + 9), "d" -> 5, "e" -> 0)
    val actual1 = Monoid.HashMap[String, Long](_ + _).op(hm1, hm2)
    assertEquals(expected, actual1)
    val actual2 = Monoid.HashMap[String, Long](Monoid.Sum[Long].op).op(hm1, hm2)
    assertEquals(expected, actual2)
  }
}
