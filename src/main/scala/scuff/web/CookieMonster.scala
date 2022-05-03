package scuff.web

import javax.servlet._
import scuff._
import scala.concurrent.duration._
import scala.util.Try
import java.time.Clock
import java.time.OffsetDateTime
import scala.annotation.nowarn

object CookieMonster {
  private val SEP = "()<>@,;:\\\"/[]?={}".toSet
  private val NotSep = {
    val isSep = (SEP.apply _)
    isSep.negate
  }

  private final val SessionDuration: FiniteDuration = -1.seconds

  sealed trait SameSite extends Enum.Value
  object SameSite extends Enum[SameSite] {
    val Lax, Strict, None, omit = new Val with SameSite
  }

  /** Convert Expires timestamp to MaxAge seconds, using current time. */
  final def toMaxAge(expires: Long, unit: TimeUnit)(
      implicit
      clock: Clock): FiniteDuration = {
    val expiresMillis = unit toMillis expires
    val diff = expiresMillis - clock.millis
    (diff / 1000).seconds
  }

  final def toMaxAge(expires: OffsetDateTime)(
      implicit
      clock: Clock): FiniteDuration =
    toMaxAge(expires.toEpochSecond, SECONDS)
  final def toMaxAge(expires: java.util.Date)(
      implicit
      clock: Clock): FiniteDuration =
    toMaxAge(expires.getTime, MILLISECONDS)

}

/**
 * Typed cookie definition.
 */
trait CookieMonster[T] {

  /**
   * Assign to `maxAge` for session cookies.
   */
  final def SessionCookie: FiniteDuration = CookieMonster.SessionDuration

  /** Max-age in seconds. Use `SessionCookie` for session cookie. */
  protected def maxAge: FiniteDuration

  protected def codec: Codec[T, String]
  def name: String

  /**
   * HTTP only cookie? Defaults to `true`.
   */
  protected def isHttpOnly = true

  protected def SameSite = CookieMonster.SameSite

  /** `SameSite` value. Defaults to `Lax`. */
  protected def sameSite: CookieMonster.SameSite = SameSite.Lax

  /**
   * Secure cookie? Defaults to `false`.
   */
  protected def isSecure = false

  /**
   * URL scope for cookie. Default is root.
   */
  protected def path: String = null

  /**
   * Domain scope for cookie.
   * Per the Cookie API: "By default, cookies are only returned to the server that sent them."
   */
  protected def domain(@nowarn req: http.HttpServletRequest): String = null

  private lazy val validName: String = {
    val name = this.name
    require(name.length > 0, "Cookie name cannot be empty")
    require(name.forall(c => c > 32 && c != 127), "Cookie name cannot contain spaces or CTL chars")
    require(name.forall(CookieMonster.NotSep), "Cookie name cannot contain separator chars")
    name
  }

  /**
   * Set value as cookie on response.
   * @param res Response object
   * @param value Cookie value
   * @param overrideMaxAge Optional Max-Age override
   * @param overridePath Optional Path override
   * @param req Implicit request object
   */
  def set(res: http.HttpServletResponse, value: T, overrideMaxAge: FiniteDuration = this.maxAge, overridePath: String = this.path)(implicit req: http.HttpServletRequest): Unit = {
    val encodedValue = codec encode value
    val cookie = new java.lang.StringBuilder(validName.length + encodedValue.length + 200)
    cookie append validName append '=' append encodedValue
    if (sameSite != SameSite.omit) cookie append "; SameSite=" append sameSite
    if (isSecure) cookie append "; Secure"
    if (isHttpOnly) cookie append "; HttpOnly"
    if (overrideMaxAge.length != SessionCookie.length) {
      cookie append "; Max-Age=" append overrideMaxAge.toSeconds
    }
    domain(req) match {
      case null => // Ignore
      case domain => cookie append "; Domain=" append domain
    }
    if (overridePath != null) cookie append "; Path=" append overridePath
    res.addHeader("Set-Cookie", cookie.toString)
  }

  /**
   * Get value from cookie on request.
   */
  def get(request: http.HttpServletRequest): Option[Try[T]] =
    get(request.getCookies)

  /**
   * Get value from available cookies.
   */
  def get(cookies: Array[http.Cookie]): Option[Try[T]] = {
    Option(cookies).flatMap { array =>
      array.find(_.getName == name).map { c =>
        Try(codec.decode(c.getValue))
      }
    }
  }

  /**
   * Remove cookie.
   */
  def remove(res: http.HttpServletResponse): Unit = {
    val cookie = new http.Cookie(name, "")
    cookie.setMaxAge(0) // Remove cookie
    res.addCookie(cookie)
  }

}

trait HmacCookieMonster[T] extends CookieMonster[T] {
  protected def hmac: Hmac[T, String]
  protected def codec = hmac
}
