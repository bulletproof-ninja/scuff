package scuff.concurrent

import org.junit._, Assert._

class TestMultiMap {
  @Test
  def `add and remove`() {
    val mmap = new MultiMap[Int, String]
    assertFalse(mmap(5) contains "foo")
    assertTrue(mmap(5) += "foo")
    assertTrue(mmap(5) contains "foo")
    assertFalse(mmap(5) += "foo")
    assertTrue(mmap(5) -= "foo")
    assertFalse(mmap(5) contains "foo")

    assertTrue(mmap(5) += "foo")
    assertTrue(mmap(5) contains "foo")
    assertTrue(mmap(5) += "bar")
    assertTrue(mmap(5) contains "bar")
    assertEquals(2, mmap(5).size)
    assertEquals(Set("foo", "bar"), mmap(5).toSet)
    mmap(5) = "foobar"
    assertFalse(mmap(5) contains "foo")
    assertFalse(mmap(5) contains "bar")
    assertEquals(1, mmap(5).size)
    for {
      value <- mmap(5)
    } assertEquals("foobar", value)
    val keys = for {
      (key, values) <- mmap
      value <- values
    } yield {
      assertEquals(5, key)
      assertEquals("foobar", value)
      key
    }
    assertEquals(5, keys.head)
  }
}
