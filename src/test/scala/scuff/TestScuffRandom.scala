package scuff

import org.junit._
import Assert._
import scala.util.Random
import scala.collection.immutable.NumericRange

class TestScuffRandom {
  //  import scuff.ScuffRandom
  @Test
  def bigint() {
    val random = new Random
    val range = BigInt(-66) to BigInt(66)
    (1 to 10000).map(_ => random.nextInRange(range)).foreach { rand =>
      assertTrue(range contains rand)
    }
  }
  def int() {
    val random = new Random
    val range = -1000 to 1000
    (1 to 10000).map(_ => random.nextInRange(range)).foreach { rand =>
      assertTrue(range contains rand)
    }
  }
  def float() {
    val random = new Random
    val range = -123.456f to 789.456f
    val set = range.by(0.0001f)
    (1 to 10000).map(_ => random.nextInRange(range)).foreach { rand =>
      assertTrue(set.contains(rand))
    }
  }
}
