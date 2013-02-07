package scuff.web

import javax.servlet._
import javax.servlet.http._
import java.security.Principal
import HttpServletResponse._
import scuff.UserPrincipal

/**
 * Application authorization trait. Apply to servlets
 * to limit access.
 */
trait Authorization extends HttpServlet {

  /** Roles allowed for this servlet */
  protected def rolesAllowed: Set[String]

  abstract override def service(req: HttpServletRequest, res: HttpServletResponse) {
    req.getUserPrincipal match {
      case null ⇒ res.setStatus(SC_UNAUTHORIZED)
      case user ⇒
        if (rolesAllowed.exists(req.isUserInRole)) {
          super.service(req, res)
        } else {
          res.setStatus(SC_FORBIDDEN)
        }
    }
  }

}

/**
 * Augment HttpServletRequest with principal and roles.
 */
abstract class ApplicationSecurityFilter extends HttpFilter {
  /**
   * Lookup authenticated user by session ID.
   * @return Authenticated user, or None if not authenticated
   */
  protected def getAuthenticatedUser(sessionID: String): Option[UserPrincipal]
  protected def logoutUser(sessionID: String)

  def doFilter(req: HttpServletRequest, res: HttpServletResponse, chain: FilterChain) {
    require(req.getUserPrincipal == null, "Another authentication mechanism is already being used: " + req.getUserPrincipal)
    val request = req.getSession(false) match {
      case null ⇒ req
      case session ⇒
        getAuthenticatedUser(session.getId) match {
          case None ⇒ req
          case Some(user) ⇒
            new HttpServletRequestWrapper(req) {
              @volatile var loggedOut = false
              override def isUserInRole(role: String) = !loggedOut && user.roles.contains(role)
              override def getUserPrincipal = if (loggedOut) null else user
              override def logout {
                logoutUser(session.getId)
                super.logout()
                loggedOut = true
              }
              override def getAuthType = classOf[Authorization].getName
              override def getRemoteUser = if (loggedOut) null else user.getName
            }
        }
    }
    chain.doFilter(request, res)
  }
}

/**
 * Apply this trait to an existing filter to get forwarding
 * (not redirect) to login page.
 */
trait AuthenticationForwarding extends HttpFilter {

  protected def loginPage: String

  abstract override def doFilter(req: HttpServletRequest, res: HttpServletResponse, chain: FilterChain) {
    super.doFilter(req, res, chain)
    if (!res.isCommitted) res.getStatus match {
      case SC_UNAUTHORIZED ⇒
        res.setStatus(SC_FOUND)
        req.getRequestDispatcher(loginPage).forward(req, res)
      case SC_FORBIDDEN ⇒ res.sendError(SC_FORBIDDEN)
      case _ ⇒ // Ignore anything else
    }
  }

}

/**
 * Standalone application of AuthenticationForwarding trait.
 * Does nothing else.
 */
abstract class AuthenticationForwardingFilter extends NoOpHttpFilter with AuthenticationForwarding
