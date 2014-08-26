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

  @Test
  def parseInt {
    assertEquals(45678, "45678".unsafeInt(offset = 0))
    assertEquals(45678, "45678".unsafeInt(offset = 0, length = 5))
    assertEquals(45678, "abc45678".unsafeInt(offset = 3))
    assertEquals(45678, "abc45678def".unsafeInt(offset = 3, length = 5))
  }

  @Test
  def parseLong {
    assertEquals(45678L, "45678".unsafeLong(offset = 0))
    assertEquals(45678L, "45678".unsafeLong(offset = 0, length = 5))
    assertEquals(45678L, "abc45678".unsafeLong(offset = 3))
    assertEquals(45678L, "abc45678def".unsafeLong(offset = 3, length = 5))
  }
}
