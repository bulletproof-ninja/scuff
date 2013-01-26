package scuff.web

import javax.servlet._
import javax.servlet.http._
import java.security.Principal
import HttpServletResponse._
import scuff.UserPrincipal

trait Authorization extends HttpServlet {

  /** Roles allowed for this servlet */
  protected def rolesAllowed: Set[String]

  /**
    * Lookup authenticated user by session ID.
    * @return Authenticated user, or None if not authenticated
    */
  protected def getAuthenticatedUser(sessionID: String): Option[UserPrincipal]

  abstract override def service(req: HttpServletRequest, res: HttpServletResponse) {
    req.getSession(false) match {
      case null ⇒ res.setStatus(SC_FORBIDDEN)
      case session ⇒
        getAuthenticatedUser(session.getId) match {
          case None ⇒ res.setStatus(SC_FORBIDDEN)
          case Some(user) ⇒
            if (rolesAllowed.intersect(user.roles).isEmpty) {
              res.setStatus(SC_UNAUTHORIZED)
            } else {
              val reqProxy = new HttpServletRequestWrapper(req) {
                override def isUserInRole(role: String) = user.roles.contains(role)
                override def getUserPrincipal = user
              }
              super.service(reqProxy, res)
            }
        }

    }
  }

}

abstract class AuthenticationRerouteFilter extends HttpFilter {

  val Mode = new Enumeration {
    val Redirect, Forward = Value
  }

  protected def loginPage: String
  protected def mode: Mode.Value

  protected def doFilter(req: HttpServletRequest, res: HttpServletResponse, chain: FilterChain) {
    chain.doFilter(req, res)
    if (!res.isCommitted) res.getStatus match {
      case SC_FORBIDDEN ⇒ mode match {
        case Mode.Forward ⇒
          res.setStatus(SC_FOUND)
          req.getRequestDispatcher(loginPage).forward(req, res)
        case Mode.Redirect ⇒
          res.sendRedirect(req.getContextPath concat loginPage)
      }
      case SC_UNAUTHORIZED ⇒ res.sendError(SC_UNAUTHORIZED)
      case _ ⇒ // Ignore
    }
  }
  //          

}
