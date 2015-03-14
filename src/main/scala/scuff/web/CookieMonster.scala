package scuff.web

import javax.servlet._
import scuff._
import scala.concurrent.duration._
import language.implicitConversions
import scala.util.Try

/**
 * Typed cookie definition.
 */
trait CookieMonster[T] {
  import java.util.concurrent.TimeUnit

  protected def clock: Clock = Clock.System

  /**
   * Use with `maxAge` for session cookies.
   */
  final val SessionDuration = -1.seconds

  /** Max-age in seconds. */
  protected def maxAge: FiniteDuration
  /** Convert Expires timestamp to MaxAge seconds, using current time. */
  protected final def toMaxAge(expires: Long, unit: TimeUnit) = new FiniteDuration(unit toSeconds clock.durationUntil(expires, unit), TimeUnit.SECONDS)
  implicit protected final def toMaxAge(expires: java.util.Date): FiniteDuration = toMaxAge(expires.getTime, TimeUnit.MILLISECONDS)
  protected def codec: Codec[T, String]
  def name: String

  /**
   * HTTP only cookie? Defaults to true.
   */
  protected def isHttpOnly = true

  /**
   * URL scope for cookie. Default is root.
   */
  protected def path: String = null

  /**
   * Domain scope for cookie.
   * Per the Cookie API: "By default, cookies are only returned to the server that sent them."
   */
  protected def domain(req: http.HttpServletRequest): String = null

  /**
   * Set value as cookie on response.
   */
  def set(res: http.HttpServletResponse, value: T, maxAge: FiniteDuration = this.maxAge, path: String = this.path)(implicit req: http.HttpServletRequest) {
    val cookie = new http.Cookie(name, codec.encode(value))
    cookie.setHttpOnly(isHttpOnly)
    cookie.setMaxAge(maxAge.toSeconds.toFloat.round)
    for (path ← Option(path)) cookie.setPath(path)
    for (domain ← Option(domain(req))) cookie.setDomain(domain)
    res.addCookie(cookie)
  }

  /**
   * Get value from cookie on request.
   */
  def get(request: http.HttpServletRequest): Option[T] = {
    Option(request.getCookies).flatMap { array =>
      array.find(_.getName == name).flatMap { c =>
        Try(codec.decode(c.getValue)).toOption
      }
    }
  }

  /**
   * Remove cookie.
   */
  def remove(res: http.HttpServletResponse) {
    val cookie = new http.Cookie(name, "")
    cookie.setMaxAge(0) // Remove cookie
    res.addCookie(cookie)
  }

}

trait HmacCookieMonster[T] extends CookieMonster[T] {
  protected def hmac: Hmac[T]
  protected final val codec = new Codec[T, String] {
    @inline
    private def base64 = Base64.RFC_4648
    def encode(a: T): String = base64.encode(hmac.encode(a)).toString
    def decode(b: String): T = hmac.decode(base64.decode(b))
  }
}
