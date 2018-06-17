package scuff.web.form

import org.junit._
import org.junit.Assert._
import java.net.URL
import util.Try

object TestParser {

  trait Beano {
    def name: Option[String]
    def url: Option[URL]
    def age: Int
    def year: Option[Int]
    def maybe: Option[Boolean]
    def perhaps: Boolean
  }

  trait Beano2 {
    def latLng: scuff.geo.Point
  }

}

class TestParser {
  import TestParser._

  @Test
  def primitive(): Unit = {
    val form = Map("maybe" -> Seq("42"))
    trait Foo {
      def maybe: Option[Int]
    }
    object Parser extends Parser[Foo]
    Parser onePass form match {
      case Right(foo) =>
        assertTrue(Try(foo.maybe.get).isFailure)
    }
    trait Bar {
      def maybe: Option[Integer]
      Parser onePass form match {
        case Right(foo) =>
          assertEquals(42, foo.maybe.get)
      }
    }
  }

  @Test
  def simple1(): Unit = {
    object Parser extends Parser[Beano]
    val form = Map("age" -> Seq("36"), "perhaps" -> Seq("false"), "name" -> Seq("FooBar"))
    Parser.onePass(form) match {
      case Left(errors) => fail("Should have no errors, but does: " + errors)
      case Right(bean) =>
        assertEquals(36, bean.age)
        assertEquals(None, bean.maybe)
        assertEquals("FooBar", bean.name.get)
    }
  }

  @Test
  def simple2(): Unit = {
    object Parser extends Parser[Beano2]
    val form = Map("latLng" -> Seq("181:-45.1234"))
    Parser.onePass(form) match {
      case Left(errors) => assertTrue(errors.contains(Problem("latLng", ProblemType.Syntax)))
      case Right(b2) => fail("Should have failed")
    }
    Parser.onePass(form.updated("latLng", Seq("-45.1234  123.4433"))) match {
      case Left(errors) => fail("Should not fail, but does: " + errors)
      case Right(b) => assertEquals(scuff.geo.Point(-45.1234, 123.4433), b.latLng)
    }
  }

  @Test
  def `must have either name or year`(): Unit = {
    object Parser extends Parser[Beano]
    val form = Map("age" -> Seq("36"), "perhaps" -> Seq("false"))
    Parser.twoPass(form) { b =>
      if (b.year.isEmpty && b.name.isEmpty) {
        Left(Set(Problem("name", ProblemType.Missing)))
      } else {
        Right(b)
      }
    } match {
      case Left(errors) =>
        assertEquals(1, errors.size)
        assertTrue(errors.contains(Problem("name", ProblemType.Missing)))
      case Right(b) => fail("Should fail because both name and year is missing")
    }
  }

  @Test
  def `string constructor`(): Unit = {
    trait Foo {
      def email: scuff.EmailAddress
    }
    val parser = new Parser[Foo]
    parser.onePass(Map("email" -> Seq("abc@def.gh"))) match {
      case Right(foo) => assertEquals("abc@def.gh", foo.email.toString)
    }
  }

  @Test
  def `factory method`(): Unit = {
    trait Foo {
      def addr: java.net.InetAddress
    }
    val parser = new Parser[Foo]
    parser.onePass(Map("addr" -> Seq("localhost"))) match {
      case Right(foo) => assertEquals("localhost", foo.addr.getHostName)
    }
  }

  @Test
  def more(): Unit = {
    trait Foo {
      def name: Seq[String]
      def nope: Seq[String]
    }
    val parser = new Parser[Foo]
    parser.onePass(Map("name" -> Seq("foo", "bar"), "baz" -> Seq("buz"))) match {
      case Right(foo) =>
        assertEquals(Seq("foo", "bar"), foo.name)
        assertEquals(Nil, foo.nope)
    }
  }

}
