package scuff.web

import javax.servlet._

trait CookieMonster {
  final val SessionOnly = -1
  type T
  /** Max age in seconds. */
  protected def maxAge: Int
  protected def transformer: scuff.Transformer[T, String]
  protected def cookieName: String
  protected def path: Option[String] = None
  protected def domain: Option[String] = None
  def set(res: http.HttpServletResponse, value: T) {
    val cookie = new http.Cookie(cookieName, transformer.forth(value))
    cookie.setMaxAge(maxAge)
    for (path ← path) cookie.setPath(path)
    for (domain ← domain) cookie.setDomain(domain)
    res.addCookie(cookie)
  }
  def get(request: http.HttpServletRequest): Option[T] = {
    Option(request.getCookies).flatMap { array ⇒
      array.find(_.getName == cookieName).map(c ⇒ transformer.back(c.getValue))
    }
  }
  def remove(res: http.HttpServletResponse) {
    val cookie = new http.Cookie(cookieName, "")
    cookie.setMaxAge(0) // Remove cookie
    res.addCookie(cookie)
  }

}
