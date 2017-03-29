package scuff.concurrent

import org.junit._
import org.junit.Assert._
import scala.collection.immutable.SortedMap

class TestLockFreeConcurrentMap {
  @Test
  def simple() = {
    val map = new LockFreeConcurrentMap[Int, String]
    assertEquals(0, map.size)
    map.put(1, "one")
    assertEquals(1, map.size)
    assertEquals("one", map.iterator.next._2)
    map += 5 -> "five"
    assertEquals("LockFreeConcurrentMap(1 -> one, 5 -> five)", map.toString)
    assertEquals(2, map.size)
    assertFalse(map.remove(1, "fifty"))
    assertEquals(2, map.size)
    assertTrue(map.remove(1, "one"))
    assertFalse(map.contains(1))
    assertEquals(1, map.size)
    map -= 5
    assertTrue(map.isEmpty)
  }
  @Test
  def sorted() = {
    val map = new LockFreeConcurrentMap[Int, String](SortedMap.empty)
    map.put(5, "five")
    map.put(2, "two")
    map.put(3, "three")
    map.put(6, "six")
    map.put(1, "one")
    map.put(4, "four")
    assertEquals("1-2-3-4-5-6", map.keys.mkString("-"))
    val snapshot = map.snapshot[SortedMap[Int, String]]
    val `123` = snapshot.range(0, 4)
    assertTrue(`123`.contains(1))
    assertTrue(`123`.contains(2))
    assertTrue(`123`.contains(3))
    assertFalse(`123`.contains(4))
    assertFalse(`123`.contains(5))
    assertFalse(`123`.contains(6))
    assertFalse(`123`.contains(0))
  }
}
