package scuff

/**
 * Monotonic sequencer.
 * <p>If your events can arrive out of sequence, this class can be used to ensure proper sequencing.
 * <p>It expects monotonically increasing sequence numbers and will not work if gaps are expected as part of the sequence number generation
 * <p>A buffer limit can be supplied to ensure that the buffer doesn't grow indefinitely if events go missing.
 * <p>NOTICE: This sequencer is not thread-safe.
 * @param consumer The pass-through consumer
 * @param expectedSeqNum The first expected sequence number
 * @param bufferLimit The buffer limit. Optional, defaults to 0, which means no limit. A buffer limit might be necessary to deal with dropped sequence numbers, otherwise the buffer will grow indefinitely.
 * @param dupeConsumer Duplicate consumer. Optional, defaults to throwing a [DuplicateSequenceReceived].
 * If a lower than expected sequence number is received, this function is called instead of the pass-through consumer.
 */
// TODO: Re-enable specialization once this is fixed: https://issues.scala-lang.org/browse/SI-4012
class MonotonicSequencer[S, T >: Null <: AnyRef](
    private val consumer: (S, T) ⇒ Unit,
    private var expectedSeqNum: S,
    private val bufferLimit: Int = 0,
    private val dupeConsumer: (S, T) ⇒ Unit = (s: S, t: T) ⇒ throw new DuplicateSequenceNumberException(s))(implicit seqType: Numeric[S], manifest: ClassManifest[T]) {

  private def incrementSeqNum() = expectedSeqNum = seqType.plus(expectedSeqNum, seqType.one)
  private def add(s: S, i: Int) = seqType.plus(s, seqType.fromInt(i))
  private def subtract(s1: S, s2: S) = seqType.toInt(seqType.minus(s1, s2))
  private def lessThanExpected(s: S) = seqType.compare(expectedSeqNum, s) > 0

  private var array = {
    val arraySize = if (bufferLimit <= 0) 16 else bufferLimit
    new NotNullArray(new Array(arraySize))
  }

  private var offset: S = _

  private val purgeHandler = (idx: Int, t: T) ⇒ {
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
        flushBuffer()
      }
    }
  }

  protected def flushBuffer() = array.purge(purgeHandler)

  private def update(seq: S, t: T) {
    val idx = subtract(seq, offset)
    if (idx >= array.capacity) {
      if (bufferLimit == 0) {
        val newSize = math.max(array.capacity * 1.5f, idx * 1.5f).asInstanceOf[Int]
        array = array.expand(new Array[T](newSize))
      } else {
        val buffer = array.toSeq((idx: Int) ⇒ add(offset, idx)) :+ seq -> t
        array.clear()
        throw new BufferCapacityExceeded(buffer.toList)
      }
    }
    array(idx) = t
  }

  protected def gapDetected(expectedSeqNum: S, actualSeqNum: S, t: T) {
    if (!array.isEmpty) array.clear()
    offset = expectedSeqNum
    update(actualSeqNum, t)
  }

  private final class NotNullArray(array: Array[T], private var size: Int = 0, private var maxIdx: Int = -1) {

    def expand(newArray: Array[T]) = {
      if (!isEmpty) {
        System.arraycopy(array, 0, newArray, 0, maxIdx + 1)
      }
      new NotNullArray(newArray, size, maxIdx)
    }

    def toSeq(toSeqNum: (Int) ⇒ S): Seq[(S, T)] = {
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

    def purge(callback: (Int, T) ⇒ Unit) {
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

class DuplicateSequenceNumberException(val seq: Any) extends RuntimeException("Duplicate sequence number received: " + seq)
