package scuff

import org.junit._, Assert._

import scala.reflect.NameTransformer
import scala.util.Random
import scala.io.Source

import json._

import java.time._

import java.util.UUID
import scala.util.Success
import scala.util.Failure
import scala.util.Try
import scala.collection.concurrent.TrieMap
import java.net.URI
import java.util.TimeZone
import java.io.File
import java.net.URL

import java.util.Locale
import java.util.Arrays


class TestJson {

  final def JSONTestSuite = "/JSONTestSuite-master"
  final def `json.org test suite` = "/json.org test suite"

  private abstract class JsonFile(path: String, fileOverride: String = null) {
    def json: String = {
      val filename = fileOverride match {
        case null => NameTransformer decode getClass.getEnclosingMethod.getName
        case file => file
      }
      val inp = getClass.getResourceAsStream(s"$path/$filename.json").require(_ != null)
      Source.fromInputStream(inp, "UTF-8").mkString
    }
    def parse = JsVal parse json
  }

  @Test
  def undefined() = {
    val obj = JsObj()
    assertEquals(JsUndefined, obj.foo)
    assertEquals(JsNum("42".bd), obj.foo || JsNum(42))
  }

  @Test
  def undefined_failure() = {
    implicit val config = JsVal.DefaultConfig.withUndefinedAccess {
      case idx: Int => sys.error(s"Index out of bounds: $idx")
      case field: String => sys.error(s"Undefined: $field")
    }
    val obj = JsObj()
    Try(obj.foo) match {
      case Success(_) => fail("Should throw exception")
      case Failure(cause) =>
        assertEquals("Undefined: foo", cause.getMessage)
    }
    Try(assertEquals(JsNum("42".bd), obj.bar || JsNum(42))) match {
      case Success(_) => fail("Should throw exception")
      case Failure(cause) =>
        assertEquals("Undefined: bar", cause.getMessage)
    }
    Try(JsArr.Empty.apply(5)) match {
      case Success(_) => fail("Should throw exception")
      case Failure(cause) =>
        assertEquals("Index out of bounds: 5", cause.getMessage)
    }
  }

  @Test
  def empty_array() = {
    assertEquals(JsArr(), JsVal parse "  [ \n  ]  ")
    assertEquals(JsArr(), JsVal parse "[]")
  }

  @Test
  def empty_obj() = {
    assertEquals(JsObj(), JsVal parse "  { \r  }  ")
    assertEquals(JsObj(), JsVal parse "{}")
  }

  @Test
  def empty_str() = {
    assertEquals(JsStr(""), JsVal parse """  ""  """)
    assertEquals(JsStr(""), JsVal parse """""""")
  }

  @Test
  def crlf() = {
    val name = "foo\rbar\\"
    val value = "bar\nfoo\b"
    val obj = JsObj(name -> value)
    assertEquals("""{"foo\rbar\\":"bar\nfoo\b"}""", obj.toJson)
    val pobj = (JsVal parse obj.toJson).asObj
    assertEquals(value, pobj(name).asStr.value)
  }

  @Test
  def escapedStringTest() = {
    val json1 = """ "\u0041BC" """
    val JsStr(abc1) = JsVal.parse(json1)
    assertEquals("ABC", abc1)
    val json2 = """ {"abc": "\u0041BC" } """
    val obj @ JsObj(_) = JsVal.parse(json2)
    val JsStr(abc2) = obj("abc")
    assertEquals("ABC", abc2)

    val json3 = """ [  "A\u0042C" ] """
    JsVal.parse(json3) match {
      case JsArr(JsStr(abc3)) =>
        assertEquals("ABC", abc3)
    }

    val json4 = """   "AB\u0043"  """
    val JsStr(abc4) = JsVal.parse(json4)
    assertEquals("ABC", abc4)
  }

  @Test
  def leadingZero() = {
    try {
      JsVal parse """{"foo":0123}"""
      fail("Should fail on leading zero")
    } catch {
      case _: MalformedJSON => // Expected
    }
  }

  @Test
  def justString() = {
    val JsStr(string) = JsVal parse """"abc""""
    assertEquals("abc", string)
  }
  @Test
  def justStringWithWS() = {
    val JsStr(string) = JsVal parse """   "abc"   """
    assertEquals("abc", string)
  }

  @Test
  def justNumber() = {
    val JsNum(i) = JsVal parse "123"
    assertEquals(123, i.intValue)
    val JsNum(f) = JsVal parse "123.999"
    assertEquals(123, f.intValue)
    val JsNum(f2) = JsVal parse "123.999"
    assertEquals(124, f2.floatValue.round)
    val JsNum(f3) = JsVal parse "0"
    assertEquals(0, f3.intValue)
    val JsNum(f4) = JsVal parse "0.123"
    assertEquals(0.123f, f4.floatValue, 0.000001)
  }
  @Test
  def justNumberWithWS() = {
    val JsNum(i) = JsVal parse " 123 "
    assertEquals(123, i.intValue)
    val JsNum(f) = JsVal parse " 123.999 "
    assertEquals(123, f.intValue)
    val JsNum(f2) = JsVal parse " 123.999 "
    assertEquals(124, f2.floatValue.round)
    val JsNum(f3a) = JsVal parse "  0"
    assertEquals(0, f3a.intValue)
    val JsNum(f3b) = JsVal parse "  0   "
    assertEquals(0, f3b.intValue)
    val JsNum(f3c) = JsVal parse "0   "
    assertEquals(0, f3c.intValue)
    val JsNum(f4a) = JsVal parse "    0.123"
    assertEquals(0.123f, f4a.floatValue, 0.000001)
    val JsNum(f4b) = JsVal parse "0.123     "
    assertEquals(0.123f, f4b.floatValue, 0.000001)
    val JsNum(f4c) = JsVal parse "   0.123    "
    assertEquals(0.123f, f4c.floatValue, 0.000001)
  }
  @Test
  def justNull() = {
    val jsNull = JsVal parse "null"
    assertEquals(JsNull, jsNull)
  }
  @Test
  def justNullWithWS() = {
    val jsNull = JsVal parse " null "
    assertEquals(JsNull, jsNull)
  }
  @Test
  def justBoolean() = {
    val JsBool(t) = JsVal parse "true"
    assertTrue(t)
    val JsBool(f) = JsVal parse "false"
    assertFalse(f)
  }
  @Test
  def justBooleanWithWS() = {
    val JsBool(t) = JsVal parse "  true "
    assertTrue(t)
    val JsBool(f) = JsVal parse " false  "
    assertFalse(f)
  }

  @Test
  def `y_string_three-byte-utf-8`() = {
    object File extends JsonFile(JSONTestSuite)
    val JsStr(string) = File.parse.asArr(0)
    assertEquals("\u0821", string)
  }

  @Test
  def `i_structure_500_nested_arrays`() = {
    object File extends JsonFile(JSONTestSuite)
    val jsVal = File.parse

      def countNesting(jsVal: JsVal, count: Int): Int = jsVal match {
        case JsArr() => count
        case JsArr(content) => countNesting(content, count + 1)
      }
    val nestingCount = countNesting(jsVal, 1)
    assertEquals(500, nestingCount)
  }

  @Test
  def `i_number_huge_exp`() = {
    object File extends JsonFile(JSONTestSuite)
    val JsArr(JsNum(num)) = File.parse
    assertTrue(num.doubleValue.isPosInfinity)
  }

  @Test
  def `y_object_string_unicode`() = {
    object File extends JsonFile(JSONTestSuite)
    File.parse match {
      case obj: JsObj =>
        val JsStr(string) = obj("title")
        assertEquals(16 + 1, string.codePoints.count.toInt)
      case other => fail(s"Should be JsObj, was $other")
    }
  }

  @Test
  def `y_object_extreme_numbers`() = {
    object File extends JsonFile(JSONTestSuite)
    File.parse match {
      case obj: JsObj =>
        val JsNum(min) = obj("min")
        val JsNum(max) = obj("max")
        assertTrue(min.doubleValue < 0)
        assertTrue(max.doubleValue > 0)
      case other => fail(s"Should be JsObj, was $other")
    }
  }

  @Test
  def `n_object_repeated_null_null`() = {
    object File extends JsonFile(JSONTestSuite)
    try {
      File.parse
      fail(s"Should have failed")
    } catch {
      case _: MalformedJSON => // Ok
    }
  }

  @Test
  def `y_string_uEscape`() = {
    object File extends JsonFile(JSONTestSuite)
    val JsArr(JsStr(string)) = File.parse
    assertEquals(4, string.length)
    assertEquals("aクリス", string)
    assertEquals("\u0061\u30af\u30EA\u30b9", string)
  }

  @Test
  def `y_string_allowed_escapes`() = {
    implicit val config = JsVal.Config(escapeSlash = true)
    object File extends JsonFile(JSONTestSuite)
    val content @ JsArr(JsStr(string)) = File.parse
    assertEquals("\"\\/\b\f\n\r\t", string)
    assertEquals(File.json, content.toJson)
  }

  @Test
  def `y_object_duplicated_key_and_value`() = {
    object File extends JsonFile(JSONTestSuite)
    File.parse match {
      case obj: JsObj =>
        val JsStr(b) = obj("a")
        assertEquals("b", b)
      case other => fail(s"Should be JsObj, was $other")
    }
  }

  @Test(expected = classOf[MalformedJSON])
  def `n_structure_trailing_#`(): Unit = {
    object File extends JsonFile(JSONTestSuite)
    File.parse
  }

  @Test(expected = classOf[MalformedJSON])
  def `n_array_newlines_unclosed`(): Unit = {
    object File extends JsonFile(JSONTestSuite)
    File.parse
  }

  @Test(expected = classOf[MalformedJSON])
  def `n_array_colon_instead_of_comma`(): Unit = {
    object File extends JsonFile(JSONTestSuite)
    File.parse
  }

  @Test(expected = classOf[MalformedJSON])
  def `n_structure_object_with_comment`(): Unit = {
    object File extends JsonFile(JSONTestSuite)
    File.parse
  }
  @Test(expected = classOf[MalformedJSON])
  def `n_object_garbage_at_end`(): Unit = {
    object File extends JsonFile(JSONTestSuite)
    File.parse
  }

  @Test
  def `y_string_null_escape`(): Unit = {
    object File extends JsonFile(JSONTestSuite)
    val JsArr(JsStr(nullStr)) = File.parse
    assertEquals(1, nullStr.length)
    assertEquals("0", nullStr(0).toHexString)
    assertEquals(0, nullStr(0))
  }

  @Test
  def `number_1000000000000000`() = {
    object File extends JsonFile(JSONTestSuite)
    val JsArr(JsNum(num)) = File.parse
    assertEquals(1000000000000000L, num.longValue)
  }

  @Test
  def `y_array_heterogeneous`() = {
    object File extends JsonFile(JSONTestSuite)
    val JsArr(jsNull, JsNum(num), JsStr(str), JsObj(map)) = File.parse
    assertEquals(JsNull, jsNull)
    assertEquals(1, num.intValue)
    assertEquals("1", str)
    assertEquals(Map.empty, map)
  }

  @Test
  def `y_object_duplicated_key`() = {
    object File extends JsonFile(JSONTestSuite)
    File.parse match {
      case obj: JsObj =>
        val JsStr(c) = obj("a")
        assertEquals("c", c)
      case other => fail(s"Should be JsObj, was $other")
    }
  }

  @Test
  def `i_number_real_neg_overflow`() = {
    object File extends JsonFile(JSONTestSuite)
    val JsArr(num @ JsNum(_)) = File.parse
    assertEquals(BigDecimal("-123123e100000"), num.toBigDec())
  }

  @Test
  def `case class`() = {

    val LocalDateCodec = new Codec[LocalDate, String] {
      private[this] val MatchLocalDate = """(\d{1,2})/(\d{1,2})/(\d{4})""".r
      def encode(d: LocalDate) = s"""${d.getMonthValue}/${d.getDayOfMonth}/${d.getYear}"""
      def decode(str: String) = str match {
        case MatchLocalDate(month, day, year) => LocalDate.of(year.toInt, month.toInt, day.toInt)
      }
    }

    val today = LocalDate.now
    abstract class Super(val name: String)
    case class Foo(age: Byte, date: LocalDate, bool: Boolean, float: Float, list: List[Int])(name: String) extends Super(name)
    val foo1 = Foo(57, today, true, 3452.874f, -234 :: 123 :: Nil)("Hank")
    val foo2 = Foo(32, today, false, Float.PositiveInfinity, Nil)("Marty")
    val json = JsVal(foo1 :: foo2 :: Nil, {
      case date: LocalDate => LocalDateCodec encode date
    }).toJson

    val JsArr(hank: JsObj, marty: JsObj) = JsVal parse json
    assertEquals("Hank", hank("name").asStr.value)
    assertEquals("Marty", marty("name").asStr.value)
    assertEquals(57, hank("age").asNum.toInt)
    assertEquals(32, marty("age").asNum.toInt)
    assertEquals(JsBool.True, hank("bool"))
    assertEquals(JsBool.False, marty("bool"))
    assertEquals(BigDecimal("3452.874"), hank.float.asNum.toBigDec())
    assertEquals(JsNum.PositiveInfinity, marty.float.asNum)
    assertEquals(List(-234, 123), hank.list.asArr.map(_.asNum.toInt).toList)
    assertEquals(Nil, marty.list.asArr.toList)
    assertEquals(today, LocalDateCodec decode marty.date.asStr.value)
  }

  @Test
  def `json.org fail tests`(): Unit = {
    val failFiles = 1 to 33
    val incorrectFails = Set(
      1, /* Not in spec */
      18 /* Not in spec */ )
    failFiles.filterNot(incorrectFails).foreach { idx =>
      object File extends JsonFile(`json.org test suite`, s"fail$idx")
      try {
        File.parse
        fail(s"""File "fail$idx.json" should fail to parse, but didn't""")
      } catch {
        case e: MalformedJSON => assertTrue(e.getMessage.length > 0)
      }
    }
    failFiles.filter(incorrectFails).foreach { idx =>
      object File extends JsonFile(`json.org test suite`, s"fail$idx")
      assertEquals(File.parse, JsVal parse File.parse.toJson)
    }
  }
  @Test
  def `json.org pass tests`(): Unit = {
    val passFiles = 1 to 3
    passFiles.foreach { idx =>
      object File extends JsonFile(`json.org test suite`, s"pass$idx")
      assertEquals(File.parse, JsVal parse File.parse.toJson)
    }
  }

  @Test
  def `implicit conversion`(): Unit = {
    import JsVal._
    assertEquals("\"hello\"", "hello".toJson)
    assertEquals("45", 45f.toJson)
    assertEquals("42", 42.000.toJson)
    assertEquals("123.456", BigDecimal("123.456").toJson)
    assertEquals("123.45600", BigDecimal("123.45600").toJson)
    assertEquals("123.000", BigDecimal("123.000").toJson)
  }

  @Test
  def `javabeans`(): Unit = {
    class Bean(name: String) {
      def getName: String = name
    }
    assertEquals("""{"name":"Hank"}""", JsVal(new Bean("Hank")).toJson)
  }

  @Test
  def `string escape`(): Unit = {
    val s1 = "\\\"escape \\uffbb\\\""
    val s2 = """\"escape \uffbb\""""
    //    assertEquals(s1, s2)
    val js1 = JsStr(s1)
    val js2 = JsStr(s2)
    val json1 = js1.toJson
    val json2 = js2.toJson
    println(json1)
    println(json2)
    val js3 = JsVal parse json1
    val js4 = JsVal parse json2
    println(js3.asStr.value)
    println(js4.asStr.value)
    assertEquals(js1, js3)
    assertEquals(js2, js4)
    assertEquals(js1.value, js3.asStr.value)
  }

  @Test
  def `numeric equality`(): Unit = {
    assertNotEquals(JsNum(42L), 42L)
    assertNotEquals(JsNum(42L), JsNum(42.00000005))
    assertEquals(JsNum(42L), JsNum(42.0))
    assertEquals(JsNum.NaN, JsNum.NaN)
    assertEquals(JsNum.NaN, JsNum(Double.NaN))
    assertEquals(JsNum.NaN, JsNum(Float.NaN))
    assertEquals(JsNum.NaN, JsNum(Float.NaN.toDouble))
    assertEquals(JsNum(BigInt("42")), JsNum(BigInt("42").underlying))
    assertEquals(JsNum(BigDecimal("42.10")), JsNum(BigDecimal("42.10000")))
    assertEquals(JsNum(BigDecimal("42.10000").underlying), JsNum(BigDecimal("42.10")))
    assertEquals(JsNum(BigDecimal("42.10").underlying), JsNum(BigDecimal("42.10000").underlying))

    assertEquals(JsNum(BigDecimal(Long.MaxValue)), JsNum(Long.MaxValue))
    assertEquals(JsNum(Long.MaxValue), JsNum(BigDecimal(Long.MaxValue)))
    assertEquals(JsNum(BigDecimal(Long.MaxValue).underlying), JsNum(Long.MaxValue))
    assertEquals(JsNum(Long.MaxValue), JsNum(BigDecimal(Long.MaxValue).underlying))
    assertEquals(JsNum(BigInt(Long.MaxValue)), JsNum(Long.MaxValue))
    assertEquals(JsNum(Long.MaxValue), JsNum(BigInt(Long.MaxValue)))
    assertEquals(JsNum(BigInt(Long.MaxValue).underlying), JsNum(Long.MaxValue))
    assertEquals(JsNum(Long.MaxValue), JsNum(BigInt(Long.MaxValue).underlying))

    assertEquals(JsNum(BigDecimal(Int.MaxValue)), JsNum(Int.MaxValue))
    assertEquals(JsNum(Int.MaxValue), JsNum(BigDecimal(Int.MaxValue)))
    assertEquals(JsNum(BigDecimal(Int.MaxValue).underlying), JsNum(Int.MaxValue))
    assertEquals(JsNum(Int.MaxValue), JsNum(BigDecimal(Int.MaxValue).underlying))
    assertEquals(JsNum(BigInt(Int.MaxValue)), JsNum(Int.MaxValue))
    assertEquals(JsNum(Int.MaxValue), JsNum(BigInt(Int.MaxValue)))
    assertEquals(JsNum(BigInt(Int.MaxValue).underlying), JsNum(Int.MaxValue))
    assertEquals(JsNum(Int.MaxValue), JsNum(BigInt(Int.MaxValue).underlying))

    assertEquals(
      JsNum(BigInt("99999999999999999999999999999999999999999999999999999999999999")),
      JsNum(BigInt("99999999999999999999999999999999999999999999999999999999999999").underlying))
    assertEquals(
      JsNum(BigDecimal("99999999999999999999999999999999999999999999999999999999999999.00")),
      JsNum(BigInt("99999999999999999999999999999999999999999999999999999999999999")))
    assertEquals(
      JsNum(BigDecimal("99999999999999999999999999999999999999999999999999999999999999.00")),
      JsNum(BigInt("99999999999999999999999999999999999999999999999999999999999999").underlying))
    assertEquals(
      JsNum(BigDecimal("99999999999999999999999999999999999999999999999999999999999999")),
      JsNum(BigInt("99999999999999999999999999999999999999999999999999999999999999").underlying))
    assertEquals(
      JsNum(BigDecimal("99999999999999999999999999999999999999999999999999999999999999.00").underlying),
      JsNum(BigInt("99999999999999999999999999999999999999999999999999999999999999")))
    assertEquals(
      JsNum(BigInt("99999999999999999999999999999999999999999999999999999999999999")),
      JsNum(BigDecimal("99999999999999999999999999999999999999999999999999999999999999.00")))
    assertEquals(
      JsNum(BigInt("99999999999999999999999999999999999999999999999999999999999999")),
      JsNum(BigDecimal("99999999999999999999999999999999999999999999999999999999999999.00").underlying))
  }

  @Test
  def `decimals around zero`(): Unit = {
    (BigDecimal("-10.123") to BigDecimal("10.456") by BigDecimal("0.001")).foreach { num =>
      var scale = num.scale
      while (scale >= 0) {
        val scaled = num.setScale(scale, BigDecimal.RoundingMode.FLOOR)
        val parsed = JsVal.parse(scaled.toString).asNum.toBigDec()
        assertEquals(scaled, parsed)
        scale -= 1
      }
    }
  }

  @Test
  def arbitrary(): Unit = {
    val json =
s"""{
  "int": 42,
  "long": ${Int.MaxValue * 2L},
  "decimal": 546342523454326546353365363.345345435345345345,
  "zero": 0,
  "clown": "🤡",
  "escapedClown": "\\ud83e\\udd21",
  "null": null,
  "true": true,
  "false": false,
  "nan": "NaN",
  "list": [1,2,3],
  "empty": {}
}"""
    println(json)
    val obj = (JsVal parse json).asObj
    assertEquals(42, obj("int").asNum.toInt)
    assertEquals(Int.MaxValue * 2L, obj("long").asNum.toLong)
    assertEquals(BigDecimal("546342523454326546353365363.345345435345345345"), obj("decimal").asNum.toBigDec())
    assertEquals(0, obj("zero").asNum.toInt)
    assertEquals("🤡", obj("clown").asStr.value)
    assertEquals("🤡", obj("escapedClown").asStr.value)
    assertEquals(JsNull, obj("null"))
    assertEquals(JsBool.True, obj("true"))
    assertEquals(JsBool.False, obj("false"))
    assertTrue(obj("nan").asNum.toDouble.isNaN)
    assertEquals(List(1,2,3), obj("list").asArr.toList.map(_.asNum.toInt))
    assertEquals(JsObj.Empty, obj("empty"))
  }

  @Test
  def numericStrings(): Unit = {
    val jsonInts = (Short.MinValue to Short.MaxValue).map(s => s""" "$s" """).mkString("[", ",", "]")
    (JsVal parse jsonInts).asArr.foreach { jsVal =>
      val short = jsVal.asNum.toInt
      assertTrue(short >= Short.MinValue)
      assertTrue(short <= Short.MaxValue)
    }
    val jsonFloats = (Short.MinValue to Short.MaxValue).map(s => s""" "$s.1230${s.abs.toInt}" """).mkString("[", ",", "]")
    (JsVal parse jsonFloats).asArr.foreach { jsVal =>
      val dbl = jsVal.asNum.toDouble
      assertTrue(dbl >= Short.MinValue - 1)
      assertTrue(dbl <= Short.MaxValue + 1)
    }
  }

  @Test
  def javaTime() = {
    case class Foo(date: LocalDate, dateTime: OffsetDateTime, instant: Instant)
    val now = Instant.now()
    val today = LocalDate from now.atOffset(ZoneOffset.UTC)
    val timestamp = OffsetDateTime from now.atOffset(ZoneOffset.UTC)
    val json = JsVal(Foo(today, timestamp, now)).toJson
    val ast = JsVal.parse(json).asObj
    val foo = Foo(
      LocalDate parse ast.date.asStr,
      OffsetDateTime parse ast.dateTime.asStr,
      Instant parse ast.instant.asStr)
    assertEquals(today, foo.date)
  }

  @Test
  def uuid() = {
    case class Foo(rando: UUID = UUID.randomUUID(), reallyRando: UUID = new UUID(Random.nextLong(), Random.nextLong()))
    val foo = Foo()
    val json = JsVal(foo).toJson
    val ast = (JsVal parse json).asObj
    val foo2 = Foo(
      UUID fromString ast.rando.asStr,
      UUID fromString ast.reallyRando.asStr)
    assertEquals(foo, foo2)
  }

  @Test
  def keyCompression(): Unit = {
    object File extends JsonFile(`json.org test suite`, s"pass1")
    val orgArr = {
      val orgJson = Iterator.fill(1000)(File.json).mkString("[", ",", "]")
      JsVal.parse(orgJson).asArr
    }
    val orgJsonCompact = orgArr.toJson
    println(orgJsonCompact)
    val dict = new TrieMap[Int, String]
    assertTrue(dict.isEmpty)
    val compressed = orgArr.compressObjKeys(dict, isConcurrentDictionary = false)
    assertFalse(dict.isEmpty)
    val keySet = dict.values.toSet
    assertEquals("Duplicate props in dictionary!", dict.size, keySet.size)
    val orgIndex: JsArr = JsArr.fromMap(dict)
    val complete = JsObj("index" -> orgIndex, "values" -> compressed)
    val compressedJson = complete.toJson
    println(compressedJson)
    val uncompLen = orgJsonCompact.length
    val compLen = compressedJson.length
    val reductionPct = ((1 - (compLen / uncompLen.toFloat)) * 100).round
    println(s"Uncompressed length: $uncompLen, compressed length: $compLen, reduction: ~$reductionPct%")
    assertTrue(orgJsonCompact.length > compressedJson.length)
    val parsedComplete = JsVal.parse(compressedJson).asObj
    val parsedIndex = parsedComplete.index.asArr
    assertEquals(orgIndex, parsedIndex)
    val arrIndex = parsedIndex.map(_.asStr.value).toArray
    assertEquals(dict.size, arrIndex.length)
    val parsedValues = parsedComplete.values.asArr
    assertEquals(compressed, parsedValues)
    val restoredArr = parsedValues.restoreCompressedObjKeys(arrIndex, false)
    assertEquals(orgArr, restoredArr)
  }

  @Test
  def cyclic_reference(): Unit = {
    case class Link(text: String, uri: URI)
    val link = Link("Google Search", URI create "https://www.google.com")
    val json = JsVal(link).toJson
    val ast = JsVal.parse(json).asObj
    assertEquals(link.text, ast.text.asStr.value)
    assertEquals(link.uri.toString, ast.uri.asStr.value)
  }

  @Test
  def common_value_classes(): Unit = {
    case class Common(
      timestamp: Timestamp = new Timestamp,
      timeZone: TimeZone = TimeZone.getDefault,
      file: File = new File(System getProperty "user.home"),
      uri: URI = new URI("https://www.amazon.com"),
      url: URL = new URL("https://www.facebook.com"),
      locale: Locale = Locale.CANADA_FRENCH)
    val common = new Common
    val json = JsVal(common).toJson
    val ast = JsVal.parse(json).asObj
    assertEquals(common.timestamp, new Timestamp(ast.timestamp.asNum))
    assertEquals(common.timeZone, TimeZone getTimeZone ast.timeZone.asStr)
    assertEquals(common.file, new File(ast.file.asStr))
    assertEquals(common.uri, new URI(ast.uri.asStr))
    assertEquals(common.url, new URL(ast.url.asStr))
    assertEquals(common.locale, Locale forLanguageTag ast.locale.asStr)
  }

  @Test
  def byte_array(): Unit = {
    val bytes = new Array[Byte](200)
    Random.nextBytes(bytes)
    val json = JsVal(bytes).toJson
    val base64 = JsVal.parse(json).asStr.value
    assertTrue(Arrays.equals(bytes, Base64.RFC_4648 decode base64))
  }

}
