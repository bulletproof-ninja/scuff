package scuff.web

import scuff._
import org.junit._
import org.junit.Assert._
import java.util.concurrent.TimeUnit
import org.mockito.Mockito._
import javax.servlet.http._
import org.mockito.ArgumentCaptor

class TestCookieMonster {
  @Test
  def `max age` {
    var expires = 9999
    object CM extends CookieMonster[String] {
      def name = "Testing"
      def codec = Codec.noop
      override val clock = new Clock {
        def precision = concurrent.duration.MILLISECONDS
        def now(implicit tu: concurrent.duration.TimeUnit) = 0
      }
      def maxAge = toMaxAge(expires, clock.precision)
    }
    assertEquals(9L, CM.maxAge.toSeconds)
  }

  @Test
  def `hmac` {
    case class User(id: Int, name: String)
    object UserCodec extends Codec[User, String] {
      def encode(u: User) = s"${u.id}:${u.name}"
      def decode(s: String) = {
        val pos = s.indexOf(':')
        User(s.substring(0, pos).toInt, s.substring(pos + 1))
      }
    }
    object UserCookie extends HmacCookieMonster[User] {
      def name = "user"
      val hmac = Hmac(UserCodec, Hmac.generateKey())
      def maxAge = SessionDuration
    }
    val user = new User(42, "Nils")
    implicit val req = mock(classOf[HttpServletRequest])
    val res = mock(classOf[HttpServletResponse])
    UserCookie.set(res, user)
    val cookieArg = ArgumentCaptor.forClass(classOf[Cookie])
    verify(res).addCookie(cookieArg.capture)
    val cookie = cookieArg.getValue
    when(req.getCookies).thenReturn(Array(cookie))
    UserCookie.get(req) match {
      case None => fail("Should return user cookie")
      case Some(cookieUser) =>
        assertNotSame(user, cookieUser)
        assertEquals(user, cookieUser)
    }
  }
}
