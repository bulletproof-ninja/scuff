package scuff.web

import javax.servlet._
import scuff._
import scala.concurrent.duration._
import language.implicitConversions

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

  /** Max-age in seconds. Defaults to session duration. */
  protected def maxAge: FiniteDuration
  /** Convert Expires timestamp to MaxAge seconds, using current time. */
  protected final def toMaxAge(expires: Long, unit: TimeUnit) = new FiniteDuration(unit toSeconds clock.durationUntil(expires)(unit), TimeUnit.SECONDS)
  implicit protected final def toMaxAge(expires: java.util.Date): FiniteDuration = toMaxAge(expires.getTime, TimeUnit.MILLISECONDS)
  def codec: Codec[T, String]
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
   * Per the Cookie API: "By default, cookies are only returned to the server that sent them. "
   */
  protected def domain: String = null

  /**
   * Set value as cookie on response.
   */
  def set(res: http.HttpServletResponse, value: T, maxAge: FiniteDuration = this.maxAge, path: String = this.path) {
    val cookie = new http.Cookie(name, codec.encode(value))
    cookie.setHttpOnly(isHttpOnly)
    cookie.setMaxAge(maxAge.toSeconds.toFloat.round)
    for (path ← Option(path)) cookie.setPath(path)
    for (domain ← Option(domain)) cookie.setDomain(domain)
    res.addCookie(cookie)
  }

  /**
   * Get value from cookie on request.
   */
  def get(request: http.HttpServletRequest): Option[T] = {
    Option(request.getCookies).flatMap { array ⇒
      array.find(_.getName == name).map(c ⇒ codec.decode(c.getValue))
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
