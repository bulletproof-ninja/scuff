package scuff

import scala.math._

/**
 * Calculate Levenshtein distance.
 * Taken, and modified, from:
 * http://rosettacode.org/wiki/Levenshtein_distance#Scala
 */
object Levenshtein {
  private def minimum(i1: Int, i2: Int, i3: Int) = min(min(i1, i2), i3)
  def distance(s1: String, s2: String) = {
    val dist = Array.tabulate(s2.length + 1, s1.length + 1) { (j, i) ⇒ if (j == 0) i else if (i == 0) j else 0 }
    for (j ← 1 to s2.length; i ← 1 to s1.length)
      dist(j)(i) = if (s2(j - 1) == s1(i - 1)) dist(j - 1)(i - 1)
      else minimum(dist(j - 1)(i) + 1, dist(j)(i - 1) + 1, dist(j - 1)(i - 1) + 1)

    dist(s2.length)(s1.length)
  }

  implicit class LevenshteinString(val s1: String) extends AnyVal {
    def levenshtein(s2: String) = Levenshtein.distance(s1, s2)
  }
}