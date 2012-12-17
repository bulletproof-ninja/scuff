package scuff

import org.junit._
import org.junit.Assert._

class TestLockFreeConcurrentMap {
  @Test
  def simple = {
    val map = new LockFreeConcurrentMap[Int, String]
    assertEquals(0, map.size)
    map.put(1, "one")
    assertEquals(1, map.size)
    assertEquals("one", map.iterator.next._2)
    map += 5 -> "five"
    assertEquals(2, map.size)
    assertFalse(map.remove(1, "fifty"))
    assertEquals(2, map.size)
    assertTrue(map.remove(1, "one"))
    assertFalse(map.contains(1))
    assertEquals(1, map.size)
  }
}