package scuff

import org.junit._
import org.junit.Assert._
import scala.util.Random
import scala.util.Try
import java.util.Arrays

class TestBase64 extends {

  val codecs = {
    import Base64._
    List(
      "RFC_1521(lineBreaks = true)" -> RFC_1521(lineBreaks = true, isSymmetric = true),
      "RFC_1521(lineBreaks = false)" -> RFC_1521(lineBreaks = false),
      "RFC_4648(withPadding = true)" -> RFC_4648(withPadding = true),
      "RFC_4648(withPadding = false)" -> RFC_4648(withPadding = false),
      "Custom('`', '~')" -> Custom('`', '~'),
      "Custom('$', '%', paddingChar = '*', maxLineLength = 50)" -> Custom('$', '%', paddingChar = '*', maxLineLength = 50, isSymmetric = true),
      "Custom('$', '%', maxLineLength = 31)" -> Custom('$', '%', maxLineLength = 31, isSymmetric = true))
  }

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
  def `full text encoding, with padding`(): Unit = {
    val codec = Base64.RFC_4648(withPadding = true)
    val encoded = codec.encode(LeviathanQuote.getBytes).toString
    assertEquals(LeviathanQuoteEncodedWithPadding, encoded)
  }
  @Test
  def `full text decoding`(): Unit = {
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
  def `20 byte encoding, with padding`(): Unit = {
    val codec = Base64.RFC_4648(withPadding = true)
    val encoded = codec.encode(LeviathanQuote.substring(LeviathanQuote.length - 20).getBytes).toString
    val expected = LeviathanQuoteEncodedWithPadding.substring(LeviathanQuoteEncodedWithPadding.length - 28)
    assertEquals(expected, encoded)
  }
  @Test
  def `empty bytes`(): Unit = {
    val codec = Base64.RFC_4648
    val encoded = codec.encode(Array.empty[Byte])
    assertEquals("", encoded)
  }
  @Test
  def `single byte encoding, no padding`(): Unit = {
    val codec = Base64.RFC_4648
    val encoded = codec.encode(LeviathanQuote.getBytes take 1).toString
    assertEquals("TQ", encoded)
  }
  @Test
  def `two byte encoding, no padding`(): Unit = {
    val codec = Base64.RFC_4648
    val encoded = codec.encode(LeviathanQuote.getBytes take 2).toString
    assertEquals("TWE", encoded)
  }
  @Test
  def `three byte encoding, no padding`(): Unit = {
    val codec = Base64.RFC_4648
    val encoded = codec.encode(LeviathanQuote.getBytes take 3).toString
    assertEquals("TWFu", encoded)
  }
  @Test
  def `four byte encoding, no padding`(): Unit = {
    val codec = Base64.RFC_4648
    val encoded = codec.encode(LeviathanQuote.getBytes take 4).toString
    assertEquals("TWFuIA", encoded)
  }
  @Test
  def `single byte encoding, with padding`(): Unit = {
    val codec = Base64.RFC_4648(withPadding = true)
    val encoded = codec.encode(LeviathanQuote.getBytes take 1).toString
    assertEquals("TQ==", encoded)
  }
  @Test
  def `two byte encoding, with padding`(): Unit = {
    val codec = Base64.RFC_4648(withPadding = true)
    val encoded = codec.encode(LeviathanQuote.getBytes take 2).toString
    assertEquals("TWE=", encoded)
  }
  @Test
  def `three byte encoding, with padding`(): Unit = {
    val codec = Base64.RFC_4648(withPadding = true)
    val encoded = codec.encode(LeviathanQuote.getBytes take 3).toString
    assertEquals("TWFu", encoded)
  }
  @Test
  def `four byte encoding, with padding`(): Unit = {
    val bytes = LeviathanQuote.getBytes take 4
    val encoded = Base64.RFC_4648(withPadding = true).encode(bytes).toString
    assertEquals("TWFuIA==", encoded)
    val encodedNoPad = Base64.RFC_4648(withPadding = false).encode(bytes).toString
    assertEquals("TWFuIA", encodedNoPad)
    val decoded = Base64.RFC_4648(withPadding = false).decode(encoded)
    val decodedNoPad = Base64.RFC_4648(withPadding = false).decode(encodedNoPad)
    assertArrayEquals(bytes, decoded)
    assertArrayEquals(bytes, decodedNoPad)
  }
  @Test
  def `custom`(): Unit = {
    val codec = Base64.Custom('%', '$', paddingChar = '_')
    val encoded = codec.encode(LeviathanQuote.getBytes)
    assertEquals('_', encoded.charAt(encoded.length - 1))
    val decoded = new String(codec.decode(encoded)).toString
    assertEquals(LeviathanQuote, decoded)
  }

  @Test
  def `randomized comparison`(): Unit = {
    import language.reflectiveCalls
    val sunEncoder = Try {
      val encoder = Class.forName("sun.misc.BASE64Encoder").newInstance
      encoder.asInstanceOf[{ def encodeBuffer(b: Array[Byte]): String }]
    }
    val codec = Base64.RFC_1521
    for (_ <- 1 to 500) {
      val bytes = new Array[Byte](Random.nextBetween(1024, 8192 + 1))
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
  def `linebreaks`(): Unit = {
    val javaEncoder = java.util.Base64.getMimeEncoder
    val codec = Base64.RFC_2045(lineBreaks = true, isSymmetric = true)
    val bytes = new Array[Byte](150)
    Random.nextBytes(bytes)
    val scuffEncoded = codec encode bytes
    val javaEncoded = new String(javaEncoder encode bytes, "US-ASCII")
    assertEquals(javaEncoded, scuffEncoded.toString.trim)
    val scuffDecoded = codec decode scuffEncoded
    assertArrayEquals(bytes, scuffDecoded)
  }

  @Test
  def `randomized data`(): Unit = {
    codecs foreach {
      case (desc, codec) =>
        for (_ <- 1 to 500) {
          val bytes = new Array[Byte](Random.nextBetween(10, 8192 + 1))
          Random.nextBytes(bytes)
          val encoded = codec encode bytes
          val decoded = codec decode encoded
          if (!Arrays.equals(bytes, decoded)) {
            fail(s"Codec failed: $desc")
          }

        }
    }
  }

  @Test
  def `remove line feeds 0a`(): Unit = {
    val str = "TWFu\nIGlz\nIGRp\nc3Rp\nbmd1\naXNo\nZWQs\nIG5v\ndCBv\nbmx5\nIGJ5\nIG\n"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\n", ""), removed)
  }
  @Test
  def `remove line feeds 0b`(): Unit = {
    val str = "TWFu\nIGlz\nIGRp\nc3Rp\nbmd1\naXNo\nZWQs\nIG5v\ndCBv\nbmx5\nIGJ5\nIG"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\n", ""), removed)
  }
  @Test
  def `remove line feeds 0c`(): Unit = {
    val str = "TWFu\nIGlz\nIGRp\nc3Rp\nbmd1\naXNo\nZWQs\nIG5v\ndCBv\nbmx5\nIGJ5\nI"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\n", ""), removed)
  }
  @Test
  def `remove line feeds 0d`(): Unit = {
    val str = "TWFu\nI"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\n", ""), removed)
  }
  @Test
  def `remove line feeds 0e`(): Unit = {
    val str = "TWFu\n"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\n", ""), removed)
  }
  @Test
  def `remove line feeds 0f`(): Unit = {
    val str = "TWFu"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\n", ""), removed)
  }

  @Test
  def `remove line feeds 1`(): Unit = {
    val str = "TWFu\r\nIGlz\r\nIGRp\r\nc3Rp\r\nbmd1\r\naXNo\r\nZWQs\r\nIG5v\r\ndCBv\r\nbmx5\r\nIGJ5\r\nIG\r\n"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
  @Test
  def `remove line feeds 2`(): Unit = {
    val str = "TWFu\r\nIGlz\r\nIGRp\r\nc3Rp\r\nbmd1\r\naXNo\r\nZWQs\r\nIG5v\r\ndCBv\r\nbmx5\r\nIGJ5\r\nIG"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
  @Test
  def `remove line feeds 3`(): Unit = {
    val str = "TWFu\r\nIGlz\r\nIGRp\r\nc3Rp\r\nbmd1\r\naXNo\r\nZWQs\r\nIG5v\r\ndCBv\r\nbmx5\r\nIGJ5\r\n"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
  @Test
  def `remove line feeds 4`(): Unit = {
    val str = "TWFu\r\nIGlz\r\nIGRp\r\nc3Rp\r\nbmd1\r\naXNo\r\nZWQs\r\nIG5v\r\ndCBv\r\nbmx5\r\nIGJ5"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
  @Test
  def `remove line feeds 5`(): Unit = {
    val str = "TWFu\r\n"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
  @Test
  def `remove line feeds 6`(): Unit = {
    val str = "TWFu"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
  @Test
  def `remove line feeds 7`(): Unit = {
    val str = "TW\r\n"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
  @Test
  def `remove line feeds 8`(): Unit = {
    val str = "TW"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
  @Test
  def `remove line feeds 9`(): Unit = {
    val str = "TWFu\r\nIGlz\r\n"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
  @Test
  def `remove line feeds 10`(): Unit = {
    val str = "TWFu\r\nIGlz"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
  @Test
  def `remove line feeds 11`(): Unit = {
    val str = "TWFu\r\nIG\r\n"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
  @Test
  def `remove line feeds 12`(): Unit = {
    val str = "TWFu\r\nIG"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
  @Test
  def `remove line feeds 13`(): Unit = {
    val str = "TWFu\r\nIG\r\n\r\n"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
  @Test
  def `remove line feeds 14`(): Unit = {
    val str = "TWFu\r\nIGlz\r\n\r\n"
    val removed = Base64.removeEOLs(str, 4).toString
    assertEquals(str.replace("\r\n", ""), removed)
  }
}
