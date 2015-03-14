package scuff

import org.junit._
import Assert._

class TestScuffAny {
  @Test
  def coerce {
    import java.awt._
    -65536.coerceTo[Color] match {
      case None => fail("Should coerce to RED")
      case Some(color) => assertEquals(Color.RED, color)
    }
    Color.BLUE.getRGB.coerceTo[Color] match {
      case None => fail("Should coerce to BLUE")
      case Some(color) => assertEquals(Color.BLUE, color)
    }
    "-16711936".coerceTo[Color] match {
      case None => fail("Should coerce to GREEN")
      case Some(color) => assertEquals(Color.GREEN, color)
    }
  }
}
