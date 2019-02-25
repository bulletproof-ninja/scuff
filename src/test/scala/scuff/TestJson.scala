package scuff

import org.junit._, Assert._
import scala.reflect.NameTransformer
import scala.io.Source
import json._
import java.time.LocalDate

class TestJson {

  final def path = "/JSONTestSuite-master"

  private abstract class JsonString {
    def json: String = {
      val filename = NameTransformer decode getClass.getEnclosingMethod.getName
      val inp = getClass.getResourceAsStream(s"$path/$filename.json").ensuring(_ != null)
      Source.fromInputStream(inp, "UTF-8").mkString
    }
    def parse = JsVal parse json
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
  }
  @Test
  def justNumberWithWS() = {
    val JsNum(i) = JsVal parse " 123 "
    assertEquals(123, i.intValue)
    val JsNum(f) = JsVal parse " 123.999 "
    assertEquals(123, f.intValue)
    val JsNum(f2) = JsVal parse " 123.999 "
    assertEquals(124, f2.floatValue.round)
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
  def `i_structure_500_nested_arrays`() = {
    object File extends JsonString
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
    object File extends JsonString
    val JsArr(JsNum(num)) = File.parse
    assertTrue(num.doubleValue.isPosInfinity)
  }

  @Test
  def `y_object_string_unicode`() = {
    object File extends JsonString
    File.parse match {
      case obj: JsObj =>
        val JsStr(string) = obj("title")
        assertEquals(16 + 1, string.codePoints.count.toInt)
      case other => fail(s"Should be JsObj, was $other")
    }
  }

  @Test
  def `y_object_extreme_numbers`() = {
    object File extends JsonString
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
    object File extends JsonString
    try {
      File.parse
      fail(s"Should have failed")
    } catch {
      case _: MalformedJSON => // Ok
    }
  }

  @Test
  def `y_string_uEscape`() = {
    object File extends JsonString
    val JsArr(JsStr(string)) = File.parse
    assertEquals(4, string.length)
    assertEquals("aクリス", string)
    assertEquals("\u0061\u30af\u30EA\u30b9", string)
  }

  @Test
  def `y_string_allowed_escapes`() = {
    object File extends JsonString
    val content @ JsArr(JsStr(string)) = File.parse
    assertEquals("\"\\/\b\f\n\r\t", string)
    assertEquals(File.json, content.toJson)
  }

  @Test
  def `n_structure_trailing_#`() = {
    object File extends JsonString
    File.parse match {
      case obj: JsObj =>
        val JsStr(b) = obj("a")
        assertEquals("b", b)
      case other => fail(s"Should be JsObj, was $other")
    }
  }
  @Test
  def `y_object_duplicated_key_and_value`() = {
    object File extends JsonString
    File.parse match {
      case obj: JsObj =>
        val JsStr(b) = obj("a")
        assertEquals("b", b)
      case other => fail(s"Should be JsObj, was $other")
    }
  }

  @Test(expected = classOf[IllegalArgumentException])
  def `n_array_newlines_unclosed`(): Unit = {
    object File extends JsonString
    File.parse
  }

  @Test(expected = classOf[IllegalArgumentException])
  def `n_array_colon_instead_of_comma`(): Unit = {
    object File extends JsonString
    File.parse
  }

  @Test(expected = classOf[IllegalArgumentException])
  def `n_structure_object_with_comment`(): Unit = {
    object File extends JsonString
    File.parse
  }
  @Test(expected = classOf[IllegalArgumentException])
  def `n_object_garbage_at_end`(): Unit = {
    object File extends JsonString
    File.parse
  }

  @Test
  def `y_string_null_escape`(): Unit = {
    object File extends JsonString
    val JsArr(JsStr(nullStr)) = File.parse
    assertEquals(1, nullStr.length)
    assertEquals("0", nullStr(0).toHexString)
    assertEquals(0, nullStr(0))
  }

  @Test
  def `number_1000000000000000`() = {
    object File extends JsonString
    val JsArr(JsNum(num)) = File.parse
    assertEquals(1000000000000000L, num.longValue)
  }

  @Test
  def `y_array_heterogeneous`() = {
    object File extends JsonString
    val JsArr(jsNull, JsNum(num), JsStr(str), JsObj(map)) = File.parse
    assertEquals(JsNull, jsNull)
    assertEquals(1, num.intValue)
    assertEquals("1", str)
    assertEquals(Map.empty, map)
  }

  @Test
  def `y_object_duplicated_key`() = {
    object File extends JsonString
    File.parse match {
      case obj: JsObj =>
        val JsStr(c) = obj("a")
        assertEquals("c", c)
      case other => fail(s"Should be JsObj, was $other")
    }
  }

  @Test
  def `i_number_real_neg_overflow`() = {
    object File extends JsonString
    val JsArr(num @ JsNum(_)) = File.parse
    assertEquals(BigDecimal("-123123e100000"), num.toBigDec)
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
    println(json)
    val JsArr(hank: JsObj, marty: JsObj) = JsVal parse json
    assertEquals("Hank", hank("name").asStr.value)
    assertEquals("Marty", marty("name").asStr.value)
    assertEquals(57, hank("age").asNum.toInt)
    assertEquals(32, marty("age").asNum.toInt)
    assertEquals(JsBool.True, hank("bool"))
    assertEquals(JsBool.False, marty("bool"))
    assertEquals(BigDecimal("3452.874"), hank.float.asNum.toBigDec)
    assertEquals(JsNum.PositiveInfinity, marty.float.asNum)
    assertEquals(List(-234, 123), hank.list.asArr.map(_.asNum.toInt).toList)
    assertEquals(Nil, marty.list.asArr.toList)
    assertEquals(today, LocalDateCodec decode marty.date.asStr.value)
  }
}
