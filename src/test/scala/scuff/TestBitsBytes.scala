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
      val arr1 = new Array[Byte](8)
      r.nextBytes(arr1)
      val l = BitsBytes.bytesToLong(arr1)
      val arr2 = BitsBytes.longToBytes(l)
      assertTrue(Arrays.equals(arr1, arr2))
    }
  }

  @Test
  def `forth and back` {
    for (_ ← 1 to 1000) {
      val l1 = r.nextLong
      val arr = BitsBytes.longToBytes(l1)
      val l2 = BitsBytes.bytesToLong(arr)
      assertEquals(l1, l2)
    }
  }

  @Test
  def long2bytes {
    for (_ ← 1 to 1000) {
      val arr = new Array[Byte](8)
      val l = r.nextLong
      val bb = ByteBuffer.allocate(8)
      bb.putLong(l)
      BitsBytes.longToBytes(l, arr)
      assertTrue(java.util.Arrays.equals(bb.array, arr))
    }
  }
  @Test
  def bytes2long {
    for (_ ← 1 to 1000) {
      val arr = new Array[Byte](8)
      r.nextBytes(arr)
      val bbLong = ByteBuffer.wrap(arr).getLong()
      val arrLong = BitsBytes.bytesToLong(arr)
      assertEquals(bbLong, arrLong)
    }
  }

}