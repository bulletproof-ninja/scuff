package scuff

import org.junit._
import org.junit.Assert._

class TestCodec extends {
  @Test
  def `ensure double reversed codec is same instance`(): Unit = {
    val intStrConverter = new Codec[Int, String] {
      def encode(i: Int) = i.toString
      def decode(s: String) = s.toInt
    }
    assertSame(intStrConverter, intStrConverter.reverse.reverse)
    assertSame(intStrConverter, intStrConverter.reverse.reverse.reverse.reverse)
  }
}
