package scuff.web

import scuff._, json._
import org.junit._
import org.junit.Assert._
import org.mockito.Mockito._
import javax.servlet.http._
import org.mockito.ArgumentCaptor
import scala.concurrent.duration._
import java.time.Clock
import java.time.Instant
import java.time.ZoneId
import java.util.concurrent.TimeUnit

class TestCookieMonster {
  @Test
  def `max age`(): Unit = {
    val expires = 9999
    object CM extends CookieMonster[String] {
      def name = "Testing"
      def codec = Codec.noop
      override val clock = Clock.fixed(Instant ofEpochMilli 0, ZoneId.of("UTC"))
      def maxAge = toMaxAge(expires, TimeUnit.MILLISECONDS)
    }
    assertEquals(9L, CM.maxAge.toSeconds)
  }

  val hmacFunc = new HmacFunction(Hmac.generateKey())

  @Test
  def `hmac`(): Unit = {
    case class User(id: Int, name: String)
    object UserCodec extends Codec[User, String] {
      def encode(u: User) = s""""${u.id}|${u.name}""""
      def decode(s: String) = {
        val pos = s.indexOf('|')
        User(s.substring(1, pos).toInt, s.substring(pos + 1, s.length - 1))
      }
    }
    object UserCookie extends HmacCookieMonster[User] {
      def name = "user"
      val hmac = Hmac.json(UserCodec, hmacFunc)
      def maxAge = 2.minutes
      override def sameSite = Strict
    }
    val user = new User(42, "Nils")
    implicit val req = mock(classOf[HttpServletRequest])
    val res = mock(classOf[HttpServletResponse])
    UserCookie.set(res, user)
    val cookieNameArg = ArgumentCaptor.forClass(classOf[String])
    val cookieContentArg = ArgumentCaptor.forClass(classOf[String])
    verify(res).addHeader(cookieNameArg.capture, cookieContentArg.capture)
    val setCookie = cookieNameArg.getValue
    assertEquals("Set-Cookie", setCookie)
    val cookieContent = cookieContentArg.getValue
    assertTrue(cookieContent contains "; SameSite=Strict")
    assertTrue(cookieContent contains "; Max-Age=120")
    val Array(cookieValue, _*) = cookieContent.substring(UserCookie.name.length+1, cookieContent.length).split("; ")
    val cookie = new Cookie(UserCookie.name, cookieValue)
    when(req.getCookies).thenReturn(Array(cookie))
    req.getCookies.find(_.getName == UserCookie.name) match {
      case None => fail("Should have cookie")
      case Some(cookie) =>
        val obj = (JsVal parse cookie.getValue).asObj
        assertEquals("42|Nils", obj.data.asStr.value)
        assertEquals(27, obj.hash.asStr.value.length)
    }
    UserCookie.get(req) match {
      case None => fail("Should return user cookie")
      case Some(cookieUser) =>
        assertNotSame(user, cookieUser)
        assertEquals(user, cookieUser)
    }
  }

  @Test(expected = classOf[RuntimeException])
  def `invalid name`(): Unit = {
    object InvalidName extends CookieMonster[Int] {
      def name = "foo:bar"
      def codec = new Codec[Int, String] {
        def encode(i: Int) = i.toString
        def decode(s: String) = s.toInt
      }
      val maxAge = 30.minutes
    }
    implicit val req = mock(classOf[HttpServletRequest])
    val res = mock(classOf[HttpServletResponse])
    InvalidName.set(res, 42)
  }
}
