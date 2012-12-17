package scuff

import org.junit._
import org.junit.Assert._

class TestSequencer {

  var seen: List[(Long, java.lang.Long)] = _
  var consumer: (Long, java.lang.Long) ⇒ Unit = _

  @Before
  def setup {
    seen = Nil
    consumer = (s: Long, t: java.lang.Long) ⇒ {
      seen = (s, t) :: seen
    }
  }

  @Test
  def `Normal sequence must work` {
    val seq = 1001L
    val sequencer = new MonotonicSequencer[Long, java.lang.Long](consumer, seq)
    for (i ← 0 until 10) {
      val s = seq + i
      sequencer.apply(s, java.lang.Long.valueOf(s))
      assertEquals(i + 1, seen.size)
    }
    assertEquals(10, seen.size)
    var expected = seq
    seen.reverse.foreach { case (s, t) ⇒ assertEquals(expected, s); assertEquals(expected, t); expected += 1 }
  }

  @Test
  def `Out-of-sequence must work` {
    val sequencer = new MonotonicSequencer[Long, java.lang.Long](consumer, 0)
    sequencer.apply(5L, 5L)
    assertTrue(seen.isEmpty)
    sequencer.apply(0L, 0L)
    assertTrue(seen.isEmpty)
    sequencer.apply(4L, 4L)
    assertTrue(seen.isEmpty)
    sequencer.apply(1L, 1L)
    assertTrue(seen.isEmpty)
    sequencer.apply(3L, 3L)
    assertTrue(seen.isEmpty)
    sequencer.apply(2L, 2L)
    assertEquals(6, seen.size)
    var expected = 0L
    seen.reverse.foreach { case (s, t) ⇒ assertEquals(expected, s); assertEquals(expected, t); expected += 1 }
  }

  @Test(expected = classOf[DuplicateSequenceNumberException])
  def `Duplicates must be detected and an exception thrown` {
    val sequencer = new MonotonicSequencer[Long, java.lang.Long](consumer, 5)
    sequencer.apply(4L, 4L)
  }

  @Test
  def `Duplicates can be intercepted instead of throwing exception` {
    var ignored = 0L
    val dupeHandler = (s: Long, t: java.lang.Long) ⇒ ignored = s
    val sequencer = new MonotonicSequencer[Long, java.lang.Long](consumer, 5, 0, dupeHandler)
    sequencer.apply(4L, 4L)
    assertEquals(4L, ignored)
  }

  @Test
  def `Exception must be thrown when buffer capacity is exceeded` {
    val capacity = 10
    val sequencer = new MonotonicSequencer[Long, java.lang.Long](consumer, 0L, capacity)
    try {
    for (s ← 1L to (capacity * 2L)) {
      sequencer.apply(s, java.lang.Long.valueOf(s))
      assertTrue(seen.size <= capacity)
    }
    } catch {
      case e: MonotonicSequencer[_,_]#BufferCapacityExceeded =>
      assertEquals(1L, e.buffer.head._1)
      var expected = 1L
      e.buffer.foreach { case (s, t) ⇒ assertEquals(expected, s); assertEquals(expected, t); expected += 1 }
    }
  }

  @Test
  def `Buffer capacity must expand` {
    val sequencer = new MonotonicSequencer[Long, java.lang.Long](consumer, 0L, 0)
    for (s ← 1L until 1024L) {
      sequencer.apply(s, java.lang.Long.valueOf(s))
      assertTrue(seen.isEmpty)
    }
    sequencer.apply(0L, 0L)
    assertEquals(1024, seen.size)
    var expected = 0L
    seen.reverse.foreach { case (s, t) ⇒ assertEquals(expected, s); assertEquals(expected, t); expected += 1 }
  }

}