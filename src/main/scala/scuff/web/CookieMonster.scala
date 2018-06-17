package scuff.web

import javax.servlet._
import scuff._
import scala.concurrent.duration._
import scala.util.Try

private object CookieMonster {
  val SEP = "()<>@,;:\\\"/[]?={}".toSet
  val NotSep = {
    val isSep = (SEP.apply _)
    isSep.negate
  }
}

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
  final def toMaxAge(epoch: Long, unit: TimeUnit) = new FiniteDuration(unit toSeconds clock.durationUntil(epoch, unit), TimeUnit.SECONDS)
  final def toMaxAge(expires: java.util.Date): FiniteDuration = toMaxAge(expires.getTime, TimeUnit.MILLISECONDS)
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

  private lazy val validName: String = {
    val name = this.name
    require(name.length > 0, "Cookie name cannot be empty")
    require(name.forall(c => c > 32 && c != 127), "Cookie name cannot contain spaces or CTL chars")
    require(name.forall(CookieMonster.NotSep), "Cookie name cannot contain separator chars")
    name
  }

  /**
    * Set value as cookie on response.
    */
  def set(res: http.HttpServletResponse, value: T, maxAge: FiniteDuration = this.maxAge, path: String = this.path)(implicit req: http.HttpServletRequest): Unit = {
    val cookie = new http.Cookie(validName, codec.encode(value))
    cookie.setHttpOnly(isHttpOnly)
    cookie.setMaxAge(maxAge.toSeconds.toFloat.round)
    for (path <- Option(path)) cookie.setPath(path)
    for (domain <- Option(domain(req))) cookie.setDomain(domain)
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
