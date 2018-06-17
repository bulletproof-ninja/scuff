package scuff.reflect

import org.junit._
import org.junit.Assert._
import java.awt.Color
import java.util.concurrent.atomic.AtomicLong

class TestDynamicConstructor {

  @Test
  def constructColor(): Unit = {
    import java.awt._
    DynamicConstructor[Color](-65536) match {
      case None => fail("Should coerce to RED")
      case Some(color) => assertEquals(Color.RED, color)
    }
    DynamicConstructor[Color](Color.BLUE.getRGB) match {
      case None => fail("Should coerce to BLUE")
      case Some(color) => assertEquals(Color.BLUE, color)
    }
    DynamicConstructor[Color](-16711936.toString) match {
      case None => fail("Should coerce to GREEN")
      case Some(color) => assertEquals(Color.GREEN, color)
    }
  }

  @Test
  def constructAtomic(): Unit = {
    import java.util.concurrent.atomic._
    DynamicConstructor[AtomicLong](5L) match {
      case None => fail("Should coerce")
      case Some(al) => assertEquals(5L, al.get)
    }
  }
}
