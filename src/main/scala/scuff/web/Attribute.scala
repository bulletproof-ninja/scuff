package scuff.web

import javax.servlet.http.HttpServletRequest

final class Attribute[T](name: String) {
  def set(req: HttpServletRequest, value: T) = req.setAttribute(name, value)
  def get(req: HttpServletRequest): Option[T] =
    req.getAttribute(name) match {
      case null => None
      case value => new Some(value.asInstanceOf[T])
    }
}
