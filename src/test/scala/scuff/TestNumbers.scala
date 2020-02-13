package scuff

import org.junit._
import Assert._
import java.nio.ByteBuffer
import java.util.Arrays

class TestNumbers {
  val r = new scala.util.Random

  @Test
  def `back and forth`(): Unit = {
    for (_ <- 1 to 1000) {
      val arrL1 = new Array[Byte](8)
      val arrI1 = new Array[Byte](4)
      r.nextBytes(arrL1)
      r.nextBytes(arrI1)
      val l = arrL1.toLong()
      val i = arrI1.toInt()
      val arrL2 = l.toByteArray()
      val arrI2 = i.toByteArray()
      assertTrue(Arrays.equals(arrL1, arrL2))
      assertTrue(Arrays.equals(arrI1, arrI2))
    }
  }

  @Test
  def `forth and back`(): Unit = {
    for (_ <- 1 to 1000) {
      val l1 = r.nextLong
      val i1 = r.nextInt
      val arrL = l1.toByteArray()
      val arrI = i1.toByteArray()
      val l2 = arrL.toLong()
      val i2 = arrI.toInt()
      assertEquals(l1, l2)
      assertEquals(i1, i2)
    }
  }

  @Test
  def long2bytes(): Unit = {
    for (_ <- 1 to 1000) {
      val arrL = new Array[Byte](8)
      val arrI = new Array[Byte](4)
      val l = r.nextLong
      val i = r.nextInt
      val bbL = ByteBuffer.allocate(8)
      val bbI = ByteBuffer.allocate(4)
      bbL.putLong(l)
      Numbers.longToBytes(l, arrL)
      bbI.putInt(i)
      Numbers.intToBytes(i, arrI)
      assertTrue(java.util.Arrays.equals(bbL.array, arrL))
      assertTrue(java.util.Arrays.equals(bbI.array, arrI))
    }
  }
  @Test
  def bytes2long(): Unit = {
    for (_ <- 1 to 1000) {
      val arrL = new Array[Byte](8)
      val arrI = new Array[Byte](4)
      r.nextBytes(arrL)
      r.nextBytes(arrI)
      val bbLong = ByteBuffer.wrap(arrL).getLong()
      val bbInt = ByteBuffer.wrap(arrI).getInt()
      val arrLong = Numbers.toLong(arrL)
      val arrInt = Numbers.toInt(arrI)
      assertEquals(bbLong, arrLong)
      assertEquals(bbInt, arrInt)
    }
  }

  @Test(expected=classOf[IllegalArgumentException])
  def empty(): Unit = {
    "".unsafeInt()
  }
  @Test
  def small(): Unit = {
    assertEquals(0, "-".unsafeInt())
    assertEquals(0L, "-".unsafeLong())
    assertEquals(0, "0".unsafeInt())
    assertEquals(0L, "0".unsafeLong())
    assertEquals(0, "-0".unsafeInt())
    assertEquals(0L, "-0".unsafeLong())
    assertEquals(-3, "-3".unsafeInt())
    assertEquals(-3L, "-3".unsafeLong())
    assertEquals(3, "03".unsafeInt())
    assertEquals(3L, "03".unsafeLong())
    assertEquals(3, "3".unsafeInt())
    assertEquals(3L, "3".unsafeLong())
    assertEquals(-3, "-03".unsafeInt())
    assertEquals(-3L, "-03".unsafeLong())
  }

  @Test
  def parsing(): Unit = {
    assertEquals(-987065L, "-987065".unsafeLong())
    assertEquals(-987065L, "abc-987065".unsafeLong(offset = 3))
    assertEquals(987065L, "987065".unsafeLong())
    assertEquals(987065, "987065".unsafeInt())
    assertEquals(987065L, "x987065".unsafeLong(offset = 1))
    assertEquals(987065, "5987065".unsafeInt(offset = 1))
    assertEquals(987065L, "987065x".unsafeLong(Numbers.NonDigit))
    assertEquals(987065, "987065/".unsafeInt(Numbers.NonDigit))
    assertEquals(-3987065, "abc-3987065/".unsafeInt(Numbers.NonDigit, offset = 3))
  }

  @Test
  def fibonnaci(): Unit = {
    Numbers.fibonacci.foreach { n =>
      println(n)
      assertTrue(n >= 0)
    }
  }
}
