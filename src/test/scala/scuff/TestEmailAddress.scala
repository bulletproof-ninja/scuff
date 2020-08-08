package scuff

import org.junit._
import org.junit.Assert._

class TestEmailAddress {

  @Test def multipleAts(): Unit = {
    try {
      new EmailAddress("hello@world@foo.bar")
      fail("Cannot have multiple @s")
    } catch {
      case _: IllegalArgumentException => // Expected
    }
  }

  @Test def startsWithDot(): Unit = {
    try {
      new EmailAddress(".hello@foo.bar")
      fail("Cannot start with dot")
    } catch {
      case _: IllegalArgumentException => // Expected
    }
  }

  @Test def multipleSuccessiveDots(): Unit = {
    try {
      new EmailAddress("hello..world@foo.bar")
      fail("Cannot have multiple successive dot")
    } catch {
      case _: IllegalArgumentException => assertFalse(EmailAddress.isValid("hello..world@foo.bar"))
    }
  }

  @Test def endsWithDot(): Unit = {
    try {
      new EmailAddress("hello.@foo.bar")
      fail("Cannot end with dot")
    } catch {
      case _: IllegalArgumentException => // Expected
    }
  }

  @Test def tooLongUser(): Unit = {
    try {
      new EmailAddress("abcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcde@foo.bar")
      fail("User part cannot exceed 64 chars")
    } catch {
      case _: IllegalArgumentException => // Expected
    }
  }

  @Test def tooShortUser(): Unit = {
    try {
      new EmailAddress("@foo.bar")
      fail("User part missing")
    } catch {
      case _: IllegalArgumentException => assertFalse(EmailAddress.isValid("@foo.bar"))
    }
  }

  @Test def justRightUser(): Unit = {
    new EmailAddress("abcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcd@foo.bar")
  }

  @Test def weirdButValid(): Unit = {
    new EmailAddress("&#!~.^|||||@foo.bar.baz.buzz")
  }

  @Test def toStr(): Unit = {
    val str = "Alien5!@mars.org"
    val ema = new EmailAddress(str)
    assertEquals(str, ema.toString)
    assertEquals((str split "@")(0), ema.user)
    assertEquals((str split "@")(1), ema.domain)
  }

  @Test def companion(): Unit = {
    val ema = EmailAddress("foo", "bar.com")
    ema match {
      case EmailAddress(user, _) if user == "bar" => fail("Not a bar")
      case EmailAddress(user, _) => assertEquals("foo", user)
    }
  }

}
