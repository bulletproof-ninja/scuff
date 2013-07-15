import java.util.Locale

package object scuff {
  import scala.math._

  implicit def DefaultClock: Clock = SystemClock

  implicit class ScuffString(val str: String) extends AnyVal {
    /**
     * Calculate Levenshtein distance.
     * Taken, and modified, from:
     * http://rosettacode.org/wiki/Levenshtein_distance#Scala
     */
    def levenshtein(s2: String): Int = {
        def minimum(i1: Int, i2: Int, i3: Int) = min(min(i1, i2), i3)
      val dist = Array.tabulate(s2.length + 1, str.length + 1) { (j, i) ⇒ if (j == 0) i else if (i == 0) j else 0 }
      for (j ← 1 to s2.length; i ← 1 to str.length)
        dist(j)(i) = if (s2(j - 1) == str(i - 1)) dist(j - 1)(i - 1)
        else minimum(dist(j - 1)(i) + 1, dist(j)(i - 1) + 1, dist(j - 1)(i - 1) + 1)

      dist(s2.length)(str.length)
    }

    /**
     * Length of string in Unicode code points.
     */
    def lengthUnicode(): Int = str.codePointCount(0, str.length)

  }

  implicit class ScuffLock(val lock: java.util.concurrent.locks.Lock) extends AnyVal {
    def whenLocked[T](code: ⇒ T): T = {
      lock.lock()
      try {
        code
      } finally {
        lock.unlock()
      }
    }

  }
}
