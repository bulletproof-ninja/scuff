package scuff

import org.junit._
import org.junit.Assert._
import java.util.Arrays
import scala.util.Random
import scala.util.Try

class TestBase64 extends {

  val LeviathanQuote = """
Man is distinguished, not only by his reason, but by this singular passion from
other animals, which is a lust of the mind, that by a perseverance of delight
in the continued and indefatigable generation of knowledge, exceeds the short
vehemence of any carnal pleasure.
""".trim.replaceAll("""[\r\n]+""", " ")
  val LeviathanQuoteEncodedWithPadding = """
TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlz
IHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2Yg
dGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGlu
dWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRo
ZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=
""".trim.replaceAll("""[\r\n]+""", "")
  @Test
  def `full text encoding, with padding` {
    val codec = Base64.RFC_4648(withPadding = true)
    val encoded = codec.encode(LeviathanQuote.getBytes).toString
    assertEquals(LeviathanQuoteEncodedWithPadding, encoded)
  }
  @Test
  def `full text decoding` {
    val codecP = Base64.RFC_4648(withPadding = true)
    val codecNP = Base64.RFC_4648(withPadding = false)
    val decodedPP = codecP.decode(LeviathanQuoteEncodedWithPadding)
    val decodedPNP = codecNP.decode(LeviathanQuoteEncodedWithPadding)
    assertEquals(LeviathanQuote, new String(decodedPP))
    assertEquals(LeviathanQuote, new String(decodedPNP))
    val decodedNPP = codecP.decode(LeviathanQuoteEncodedWithPadding.replace("=", ""))
    val decodedNPNP = codecNP.decode(LeviathanQuoteEncodedWithPadding.replace("=", ""))
    assertEquals(LeviathanQuote, new String(decodedNPP))
    assertEquals(LeviathanQuote, new String(decodedNPNP))
  }

  @Test
  def `20 byte encoding, with padding` {
    val codec = Base64.RFC_4648(withPadding = true)
    val encoded = codec.encode(LeviathanQuote.substring(LeviathanQuote.length - 20).getBytes).toString
    val expected = LeviathanQuoteEncodedWithPadding.substring(LeviathanQuoteEncodedWithPadding.length - 28)
    assertEquals(expected, encoded)
  }
  @Test
  def `empty bytes` {
    val codec = Base64.RFC_4648
    val encoded = codec.encode(Array.empty[Byte])
    assertEquals("", encoded)
  }
  @Test
  def `single byte encoding, no padding` {
    val codec = Base64.RFC_4648
    val encoded = codec.encode(LeviathanQuote.getBytes take 1).toString
    assertEquals("TQ", encoded)
  }
  @Test
  def `two byte encoding, no padding` {
    val codec = Base64.RFC_4648
    val encoded = codec.encode(LeviathanQuote.getBytes take 2).toString
    assertEquals("TWE", encoded)
  }
  @Test
  def `three byte encoding, no padding` {
    val codec = Base64.RFC_4648
    val encoded = codec.encode(LeviathanQuote.getBytes take 3).toString
    assertEquals("TWFu", encoded)
  }
  @Test
  def `four byte encoding, no padding` {
    val codec = Base64.RFC_4648
    val encoded = codec.encode(LeviathanQuote.getBytes take 4).toString
    assertEquals("TWFuIA", encoded)
  }
  @Test
  def `single byte encoding, with padding` {
    val codec = Base64.RFC_4648(withPadding = true)
    val encoded = codec.encode(LeviathanQuote.getBytes take 1).toString
    assertEquals("TQ==", encoded)
  }
  @Test
  def `two byte encoding, with padding` {
    val codec = Base64.RFC_4648(withPadding = true)
    val encoded = codec.encode(LeviathanQuote.getBytes take 2).toString
    assertEquals("TWE=", encoded)
  }
  @Test
  def `three byte encoding, with padding` {
    val codec = Base64.RFC_4648(withPadding = true)
    val encoded = codec.encode(LeviathanQuote.getBytes take 3).toString
    assertEquals("TWFu", encoded)
  }
  @Test
  def `four byte encoding, with padding` {
    val bytes = LeviathanQuote.getBytes take 4
    val encoded = Base64.RFC_4648(withPadding = true).encode(bytes).toString
    assertEquals("TWFuIA==", encoded)
    val decoded = Base64.RFC_4648(withPadding = false).decode(encoded)
    assertArrayEquals(bytes, decoded)
  }
  @Test
  def `custom` {
    val codec = Base64.Custom('%', '$', withPadding = true, paddingChar = '_')
    val encoded = codec.encode(LeviathanQuote.getBytes)
    assertEquals('_', encoded.charAt(encoded.length - 1))
    val decoded = new String(codec.decode(encoded)).toString
    assertEquals(LeviathanQuote, decoded)
  }
  @Test
  def `randomized` {
    import language.reflectiveCalls
    val sunEncoder = Try {
      val encoder = Class.forName("sun.misc.BASE64Encoder").newInstance
      encoder.asInstanceOf[{ def encodeBuffer(b: Array[Byte]): String }]
    }
    val codec = Base64.RFC_1521
    val sizeRange = 1024 to 8192
    for (_ <- 1 to 500) {
      val bytes = new Array[Byte](Random.nextInRange(sizeRange))
      Random.nextBytes(bytes)
      val encoded = Base64.removeEOLs(codec.encode(bytes), 76)
      val decoded = codec.decode(encoded)
      assertArrayEquals(bytes, decoded)
      sunEncoder.foreach { sun =>
        val sunEncoded = Base64.removeEOLs(sun.encodeBuffer(bytes), 76)
        assertEquals(sunEncoded.toString, encoded.toString)
        val sunEncodedDecoded = codec.decode(sunEncoded)
        assertArrayEquals(bytes, sunEncodedDecoded)
      }
    }
  }

  @Test
  def `remove line feeds 0a` {
    val str = "TWFu\nIGlz\nIGRp\nc3Rp\nbmd1\naXNo\nZWQs\nIG5v\ndCBv\nbmx5\nIGJ5\nIG\n"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\n", ""), removed)
  }
  @Test
  def `remove line feeds 0b` {
    val str = "TWFu\nIGlz\nIGRp\nc3Rp\nbmd1\naXNo\nZWQs\nIG5v\ndCBv\nbmx5\nIGJ5\nIG"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\n", ""), removed)
  }
  @Test
  def `remove line feeds 0c` {
    val str = "TWFu\nIGlz\nIGRp\nc3Rp\nbmd1\naXNo\nZWQs\nIG5v\ndCBv\nbmx5\nIGJ5\nI"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\n", ""), removed)
  }
  @Test
  def `remove line feeds 0d` {
    val str = "TWFu\nI"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\n", ""), removed)
  }
  @Test
  def `remove line feeds 0e` {
    val str = "TWFu\n"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\n", ""), removed)
  }
  @Test
  def `remove line feeds 0f` {
    val str = "TWFu"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\n", ""), removed)
  }


  @Test
  def `remove line feeds 1` {
    val str = "TWFu\r\nIGlz\r\nIGRp\r\nc3Rp\r\nbmd1\r\naXNo\r\nZWQs\r\nIG5v\r\ndCBv\r\nbmx5\r\nIGJ5\r\nIG\r\n"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
  @Test
  def `remove line feeds 2` {
    val str = "TWFu\r\nIGlz\r\nIGRp\r\nc3Rp\r\nbmd1\r\naXNo\r\nZWQs\r\nIG5v\r\ndCBv\r\nbmx5\r\nIGJ5\r\nIG"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
  @Test
  def `remove line feeds 3` {
    val str = "TWFu\r\nIGlz\r\nIGRp\r\nc3Rp\r\nbmd1\r\naXNo\r\nZWQs\r\nIG5v\r\ndCBv\r\nbmx5\r\nIGJ5\r\n"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
  @Test
  def `remove line feeds 4` {
    val str = "TWFu\r\nIGlz\r\nIGRp\r\nc3Rp\r\nbmd1\r\naXNo\r\nZWQs\r\nIG5v\r\ndCBv\r\nbmx5\r\nIGJ5"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
  @Test
  def `remove line feeds 5` {
    val str = "TWFu\r\n"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
  @Test
  def `remove line feeds 6` {
    val str = "TWFu"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
  @Test
  def `remove line feeds 7` {
    val str = "TW\r\n"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
  @Test
  def `remove line feeds 8` {
    val str = "TW"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
  @Test
  def `remove line feeds 9` {
    val str = "TWFu\r\nIGlz\r\n"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
  @Test
  def `remove line feeds 10` {
    val str = "TWFu\r\nIGlz"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
  @Test
  def `remove line feeds 11` {
    val str = "TWFu\r\nIG\r\n"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
  @Test
  def `remove line feeds 12` {
    val str = "TWFu\r\nIG"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
  @Test
  def `remove line feeds 13` {
    val str = "TWFu\r\nIG\r\n\r\n"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
  @Test
  def `remove line feeds 14` {
    val str = "TWFu\r\nIGlz\r\n\r\n"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
}
