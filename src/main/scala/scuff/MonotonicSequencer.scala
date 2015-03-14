package scuff

import reflect.ClassTag

/**
 * Monotonic sequencer.
 * <p>If your events/messages/whatever can arrive out of sequence, this class can be used to ensure proper sequencing.
 * <p>It expects monotonically increasing sequence numbers and will not work if gaps are expected as part of the sequence number generation
 * <p>A buffer limit can be supplied to ensure that the buffer doesn't grow indefinitely if events go missing.
 * <p>NOTICE: This sequencer is not thread-safe.
 * @param consumer The pass-through consumer
 * @param expectedSeqNum The first expected sequence number
 * @param bufferLimit The buffer limit. Optional, defaults to 0, which means no limit.
 * A buffer limit might be necessary to deal with dropped sequence numbers,
 * otherwise the buffer will grow indefinitely.
 * @param gapHandler Callback interface that is notified when gaps are detected and when closed
 * @param dupeConsumer Duplicate consumer. Optional, defaults to throwing a [[MonotonicSequencer.DuplicateSequenceNumberException]].
 * If a lower than expected sequence number is received, this function is called instead of the pass-through consumer.
 */
final class MonotonicSequencer[@specialized(Int, Long) S, T >: Null <: AnyRef](
    consumer: (S, T) => Unit,
    private var expectedSeqNum: S,
    bufferLimit: Int = 0,
    gapHandler: MonotonicSequencer.GapHandler[S] = MonotonicSequencer.NoOpGapHandler[S],
    dupeConsumer: (S, T) => Unit = (s: S, t: T) => throw new MonotonicSequencer.DuplicateSequenceNumberException(s))(implicit seqType: Numeric[S], manifest: ClassTag[T]) {

  /**
   * @param consumer The pass-through consumer
   * @param expectedSeqNum The first expected sequence number
   * @param bufferLimit The buffer limit. Optional, defaults to 0, which means no limit. A buffer limit might be necessary to deal with dropped sequence numbers,
   * otherwise the buffer will grow indefinitely.
   * @param dupeConsumer Duplicate consumer. Optional, defaults to throwing a [[MonotonicSequencer.DuplicateSequenceNumberException]].
   * If a lower than expected sequence number is received, this function is called instead of the pass-through consumer.
   */
  def this(consumer: (S, T) => Unit, expectedSeqNum: S, bufferLimit: Int, dupeConsumer: (S, T) => Unit)(implicit seqType: Numeric[S], manifest: ClassTag[T]) =
    this(consumer, expectedSeqNum, bufferLimit, MonotonicSequencer.NoOpGapHandler[S], dupeConsumer)

  private def incrementSeqNum() = expectedSeqNum = seqType.plus(expectedSeqNum, seqType.one)
  private def add(s: S, i: Int) = seqType.plus(s, seqType.fromInt(i))
  private def subtract(s1: S, s2: S) = seqType.toInt(seqType.minus(s1, s2))
  private def lessThanExpected(s: S) = seqType.compare(expectedSeqNum, s) > 0

  private var array = {
    val arraySize = if (bufferLimit <= 0) 16 else bufferLimit
    new NotNullArray(new Array(arraySize))
  }

  private var offset: S = _

  private val purgeHandler = (idx: Int, t: T) => {
    val s = add(offset, idx)
    consumer(s, t)
  }

  def apply(s: S, t: T) {
    if (array.isEmpty) {
      if (s == expectedSeqNum) {
        incrementSeqNum()
        consumer(s, t)
      } else if (lessThanExpected(s)) {
        dupeConsumer(s, t)
      } else {
        gapDetected(expectedSeqNum, s, t)
      }
    } else {
      update(s, t)
      if (array.isSequenced) {
        gapClosed()
      }
    }
  }

  private def gapClosed() {
    array.purge(purgeHandler)
    gapHandler.gapClosed()
  }

  private def update(seq: S, t: T) {
    val idx = subtract(seq, offset)
    if (idx >= array.capacity) {
      if (bufferLimit == 0) {
        val newSize = math.max(array.capacity * 1.5f, idx * 1.5f).asInstanceOf[Int]
        array = array.expand(new Array[T](newSize))
      } else {
        val buffer = array.toSeq((idx: Int) => add(offset, idx)) :+ seq -> t
        array.clear()
        throw new BufferCapacityExceeded(buffer.toList)
      }
    }
    array(idx) = t
  }

  private def gapDetected(expectedSeqNum: S, actualSeqNum: S, t: T) {
    if (!array.isEmpty) array.clear()
    offset = expectedSeqNum
    update(actualSeqNum, t)
    gapHandler.gapDetected(expectedSeqNum, actualSeqNum)
  }

  private final class NotNullArray(array: Array[T], private var size: Int = 0, private var maxIdx: Int = -1) {

    def expand(newArray: Array[T]) = {
      if (!isEmpty) {
        System.arraycopy(array, 0, newArray, 0, maxIdx + 1)
      }
      new NotNullArray(newArray, size, maxIdx)
    }

    def toSeq(toSeqNum: (Int) => S): Seq[(S, T)] = {
      var i = 0
      val buffer = collection.mutable.Buffer[(S, T)]()
      while (i < array.length) {
        val t = array(i)
        if (t != null) {
          val s = toSeqNum(i)
          buffer += s -> t
        }
        i += 1
      }
      buffer
    }

    @annotation.tailrec
    def clear(i: Int = 0) {
      if (i < array.length) {
        array(i) = null
        clear(i + 1)
      }
    }

    def purge(callback: (Int, T) => Unit) {
        @annotation.tailrec
        def purge(i: Int = 0) {
          if (i < size) {
            callback(i, array(i))
            array(i) = null
            purge(i + 1)
          }
        }
      purge()
      size = 0
      maxIdx = -1
    }

    def capacity = array.length

    def isEmpty = size == 0

    def isSequenced = {
      maxIdx + 1 == size && size != 0
    }

    def update(i: Int, t: T) = {
      if (t == null) throw new IllegalArgumentException("Reference cannot be null")

      if (array(i) == null) {
        size += 1
      }
      maxIdx = math.max(maxIdx, i)
      array(i) = t
    }
  }

  /**
   * Buffer capacity was exceeded. The supplied buffer contains events received, including the event
   * that caused the capacity breach, in ordered sequence.
   */
  class BufferCapacityExceeded(val buffer: List[(S, T)]) extends RuntimeException("Buffer capacity exceeded")

}

object MonotonicSequencer {
  trait GapHandler[@specialized(Int, Long) S] {
    def gapDetected(expectedSeqNum: S, actualSeqNum: S): Unit
    def gapClosed(): Unit
  }
  def NoOpGapHandler[S] = new GapHandler[S] {
    def gapDetected(expectedSeqNum: S, actualSeqNum: S): Unit = {}
    def gapClosed(): Unit = {}
  }

  class DuplicateSequenceNumberException(val seq: Any) extends RuntimeException("Duplicate sequence number received: " + seq)
}
