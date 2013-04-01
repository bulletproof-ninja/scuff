package scuff.web

import javax.servlet._

/**
 * Typed cookie definition.
 */
trait CookieMonster {
  type T

  /**
   * Use with `maxAge` for session cookies.
   */
  final val SessionOnly = -1

  /** Max age in seconds. */
  protected def maxAge: Int
  protected def toMaxAge(expires: Long): Int = ((expires - System.currentTimeMillis) / 1000).asInstanceOf[Int]
  protected def transformer: scuff.Transformer[T, String]
  protected def cookieName: String
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
  def set(res: http.HttpServletResponse, value: T) {
    val cookie = new http.Cookie(cookieName, transformer.forth(value))
    cookie.setMaxAge(maxAge)
    for (path ← Option(path)) cookie.setPath(path)
    for (domain ← Option(domain)) cookie.setDomain(domain)
    res.addCookie(cookie)
  }

  /**
   * Get value from cookie on request. 
   */
  def get(request: http.HttpServletRequest): Option[T] = {
    Option(request.getCookies).flatMap { array ⇒
      array.find(_.getName == cookieName).map(c ⇒ transformer.back(c.getValue))
    }
  }

  /**
   * Remove cookie. 
   */
  def remove(res: http.HttpServletResponse) {
    val cookie = new http.Cookie(cookieName, "")
    cookie.setMaxAge(0) // Remove cookie
    res.addCookie(cookie)
  }

}
