package scuff

import org.junit._
import Assert._

class TestScuffString {
  @Test
  def substringEq() {
    assertTrue("abcdefg".substringEquals(0, "abc"))
    assertTrue("abcdefg".substringEquals(1, "bcd"))
    assertTrue("abcdefg".substringEquals(4, "efg"))
    assertFalse("abcdefg".substringEquals(0, "bcd"))
    assertFalse("abcdefg".substringEquals(99, "bcd"))
    assertFalse("abcdefg".substringEquals(0, "abcdefgh"))
    assertTrue("abcdefg".substringEquals(3, ""))
    assertTrue("abcdefg".substringEquals(0, "abcdefg"))
    assertFalse("abcdefg".substringEquals(1, "abcdef"))
    assertTrue("abcdefg".substringEquals(0, "abcdef"))
  }
}
