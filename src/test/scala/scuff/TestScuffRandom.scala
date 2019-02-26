package scuff

import org.junit._
import Assert._
import scala.util.Random

class TestScuffRandom {

  @Test
  def bigint(): Unit = {
    val random = new Random
    val range = BigInt(-66) until BigInt(67)
    (1 to 10000).map(_ => random.nextBetween(range.head, range.end)).foreach { rand =>
      assertTrue(rand >= BigInt(-66))
      assertTrue(rand < BigInt(67))
    }
  }

  def int(): Unit = {
    val random = new Random
    (1 to 10000).map(_ => random.nextBetween(-1000 -> 1001)).foreach { rand =>
      assertTrue(rand >= -1000)
      assertTrue(rand < 1001)
    }
  }

  def float(): Unit = {
    val random = new Random
    (1 to 10000).map(_ => random.nextBetween(-123.456, 789.456)).foreach { rand =>
      assertTrue(rand >= -123.456)
      assertTrue(rand < 789.456)
    }
  }

  def long(): Unit = {
    val random = new Random
    (1 to 100000).map(_ => random.nextBetween(1L, 5L)).foreach { rand =>
      assertTrue(rand >= 1L)
      assertTrue(rand < 5L)
    }
  }

}
