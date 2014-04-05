package scuff

import org.junit._
import Assert._

class TestScuffString {
  @Test
  def substringEq() {
    assertTrue("abcdefg".offsetStartsWith(0, "abc"))
    assertTrue("abcdefg".offsetStartsWith(1, "bcd"))
    assertTrue("abcdefg".offsetStartsWith(4, "efg"))
    assertFalse("abcdefg".offsetStartsWith(0, "bcd"))
    assertFalse("abcdefg".offsetStartsWith(99, "bcd"))
    assertFalse("abcdefg".offsetStartsWith(0, "abcdefgh"))
    assertTrue("abcdefg".offsetStartsWith(3, ""))
    assertTrue("abcdefg".offsetStartsWith(0, "abcdefg"))
    assertFalse("abcdefg".offsetStartsWith(1, "abcdef"))
    assertTrue("abcdefg".offsetStartsWith(0, "abcdef"))
  }
}
