package scuff.web

import scuff._

import org.junit._
import org.junit.Assert._

class TestCookieMonster {
  @Test
  def `max age` {
    var expires = 9999
    object CM extends CookieMonster {
      def name = "Testing"
      type T = String
      def codec = Codec.noop
      override val clock = new Clock {
        def precision = concurrent.duration.MILLISECONDS
        def now(implicit tu: concurrent.duration.TimeUnit) = 0
      }
      def maxAge = toMaxAge(expires, clock.precision)
    }
    assertEquals(9, CM.maxAge)
  }
}