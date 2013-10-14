package scuff

import org.junit._
import Assert._
import java.nio.ByteBuffer
import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream
import java.util.Arrays

class TestBitsBytes {
  val r = new scala.util.Random

  @Test
  def `back and forth` {
    for (_ ← 1 to 1000) {
      val arrL1 = new Array[Byte](8)
      val arrI1 = new Array[Byte](4)
      r.nextBytes(arrL1)
      r.nextBytes(arrI1)
      val l = BitsBytes.bytesToLong(arrL1)
      val i = BitsBytes.bytesToInt(arrI1)
      val arrL2 = BitsBytes.longToBytes(l)
      val arrI2 = BitsBytes.intToBytes(i)
      assertTrue(Arrays.equals(arrL1, arrL2))
      assertTrue(Arrays.equals(arrI1, arrI2))
    }
  }

  @Test
  def `forth and back` {
    for (_ ← 1 to 1000) {
      val l1 = r.nextLong
      val i1 = r.nextInt
      val arrL = BitsBytes.longToBytes(l1)
      val arrI = BitsBytes.intToBytes(i1)
      val l2 = BitsBytes.bytesToLong(arrL)
      val i2 = BitsBytes.bytesToInt(arrI)
      assertEquals(l1, l2)
      assertEquals(i1, i2)
    }
  }

  @Test
  def long2bytes {
    for (_ ← 1 to 1000) {
      val arrL = new Array[Byte](8)
      val arrI = new Array[Byte](4)
      val l = r.nextLong
      val i = r.nextInt
      val bbL = ByteBuffer.allocate(8)
      val bbI = ByteBuffer.allocate(4)
      bbL.putLong(l)
      BitsBytes.longToBytes(l, arrL)
      bbI.putInt(i)
      BitsBytes.intToBytes(i, arrI)
      assertTrue(java.util.Arrays.equals(bbL.array, arrL))
      assertTrue(java.util.Arrays.equals(bbI.array, arrI))
    }
  }
  @Test
  def bytes2long {
    for (_ ← 1 to 1000) {
      val arrL = new Array[Byte](8)
      val arrI = new Array[Byte](4)
      r.nextBytes(arrL)
      r.nextBytes(arrI)
      val bbLong = ByteBuffer.wrap(arrL).getLong()
      val bbInt = ByteBuffer.wrap(arrI).getInt()
      val arrLong = BitsBytes.bytesToLong(arrL)
      val arrInt = BitsBytes.bytesToInt(arrI)
      assertEquals(bbLong, arrLong)
      assertEquals(bbInt, arrInt)
    }
  }

}