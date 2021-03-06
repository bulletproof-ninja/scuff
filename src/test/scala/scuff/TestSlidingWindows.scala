package scuff

import org.junit._
import org.junit.Assert._
import scala.concurrent.duration._
import scala.concurrent.duration.DurationInt
import scuff.concurrent.ScuffScalaFuture

class TestSlidingWindows {
  import SlidingWindow._
  implicit def Dec(d: Double) = BigDecimal(d)
  implicit def toBD(i: Int) = BigDecimal(i)

  @Test
  def counting(): Unit = {
    counting(TreeMapProvider)
    counting(HashMapProvider)
    counting(LongMapProvider)
      def counting(sp: StoreProvider[Int]): Unit = {
        object Count extends Sum[Int]
        val windows = Set(Duration.Inf, 10.milliseconds, 100.milliseconds, 1.second).map(Window(_))
        val counts = new SlidingWindow(Count, windows, sp)
        (1 to 50).foreach(i => counts.add(1, i))
        val count50ms = counts.snapshot(50).await(10.seconds).map(e => e._1.length -> e._2)
        assertEquals(10, count50ms(10.milliseconds))
        assertEquals(50, count50ms(100.milliseconds))
        assertEquals(50, count50ms(1.second))
        assertEquals(50, count50ms(Duration.Inf))
        (51 to 1000).foreach(i => counts.add(1, i))
        val count1s = counts.snapshot(1000).await(10.seconds).map(e => e._1.length -> e._2)
        assertEquals(10, count1s(10.milliseconds))
        assertEquals(100, count1s(100.milliseconds))
        assertEquals(1000, count1s(1.second))
        assertEquals(1000, count1s(Duration.Inf))
        (1001 to 2000).foreach(i => counts.add(1, i))
        val count2100ms = counts.snapshot(2100).await(10.seconds).map(e => e._1.length -> e._2)
        assertEquals(0, count2100ms(10.milliseconds))
        assertEquals(0, count2100ms(100.milliseconds))
        assertEquals(900, count2100ms(1.second))
        assertEquals(2000, count2100ms(Duration.Inf))
      }
  }

  @Test
  def average(): Unit = {
    average(TreeMapProvider)
    average(HashMapProvider)
    average(LongMapProvider)
      def average(sp: StoreProvider[(BigDecimal, Int)]): Unit = {
        val windows = Set(Duration.Inf, 10.milliseconds, 100.milliseconds, 1.second).map(Window(_))
        val averages = new SlidingWindow(Average[BigDecimal], windows, sp)
        (1 to 50).foreach(i => averages.add(i, i))
        val avg50ms = averages.snapshot(50).await(10.seconds).iterator.flatMap(e => e._2.map(v => e._1.length -> v)).toMap
        assertEquals(Dec(45.5), avg50ms(10.milliseconds))
        assertEquals(Dec(25.5), avg50ms(100.milliseconds))
        assertEquals(Dec(25.5), avg50ms(1.second))
        assertEquals(Dec(25.5), avg50ms(Duration.Inf))
        (51 to 1000).foreach(t => averages.add(t, t))
        val avg1s = averages.snapshot(1000).await(10.seconds).iterator.flatMap(e => e._2.map(v => e._1.length -> v)).toMap
        assertEquals(Dec(995.5), avg1s(10.milliseconds))
        assertEquals(Dec(950.5), avg1s(100.milliseconds))
        assertEquals(Dec(500.5), avg1s(1.second))
        assertEquals(Dec(500.5), avg1s(Duration.Inf))
        (1001 to 2000).foreach(t => averages.add(t, t))
        val avg2100ms = averages.snapshot(2100).await(10.seconds).iterator.flatMap(e => e._2.map(v => e._1.length -> v)).toMap
        assertEquals(None, avg2100ms.get(10.milliseconds))
        assertEquals(None, avg2100ms.get(100.milliseconds))
        assertEquals(Dec(1550.5), avg2100ms(1.second))
        assertEquals(Dec(1000.5), avg2100ms(Duration.Inf))
        val oneThirdHundredth = Dec(33.333333333) // Is that even a word?
        (5001 to 5003).foreach(t => averages.add(oneThirdHundredth, t))
        val avg5005ms = averages.snapshot(5005).await(10.seconds).iterator.flatMap(e => e._2.map(v => e._1.length -> v)).toMap
        assertEquals(oneThirdHundredth, avg5005ms(10.milliseconds))
      }
  }

  @Test
  def `with offset`(): Unit = {
    withOffset(TreeMapProvider)
    withOffset(HashMapProvider)
    withOffset(LongMapProvider)
      def withOffset(sp: StoreProvider[Int]): Unit = {
        val last10ms = Window(10.milliseconds)
        val prev10ms = Window(10.milliseconds, 10.milliseconds)
        val sums = SlidingWindow(Sum[Int], sp, last10ms, prev10ms)
        sums.add(5, 1)
        sums.add(2, 2)
        sums.add(4, 3)
        sums.add(8, 4)
        sums.add(1, 5)
        sums.add(2, 5)
        val at7ms = sums.snapshot(7).await(10.seconds)
        assertEquals(0, at7ms(prev10ms))
        assertEquals(22, at7ms(last10ms))
        sums.add(9, 8)
        sums.add(3, 9)
        sums.add(7, 10)
        sums.add(3, 11)
        sums.add(2, 12)
        sums.add(1, 12)
        val at13ms = sums.snapshot(13).await(10.seconds)
        assertEquals(11, at13ms(prev10ms))
        assertEquals(36, at13ms(last10ms))
      }
  }
}
