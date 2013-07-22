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

  /**
    * Roles allowed for this servlet. Default allows 
    * all authenticated users, regardless of role.
    */
  protected def rolesAllowed: Set[String] = Set.empty

  abstract override def service(req: HttpServletRequest, res: HttpServletResponse) {
    req.getUserPrincipal match {
      case null ⇒ res.setStatus(SC_UNAUTHORIZED)
      case user ⇒
        if (rolesAllowed.isEmpty || rolesAllowed.exists(req.isUserInRole)) {
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
abstract class ApplicationSecurityFilter extends Filter {
  /**
   * Lookup authenticated user by request.
   * @return Authenticated user, or None if not authenticated
   */
  protected def getAuthenticatedUser(req: HttpServletRequest): Option[UserPrincipal]
  protected def logoutUser(req: HttpServletRequest, res: HttpServletResponse)

  def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain) = (req, res) match {
    case (req: HttpServletRequest, res: HttpServletResponse) ⇒ httpFilter(req, res, chain)
    case _ ⇒ chain.doFilter(req, res)
  }

  private def httpFilter(req: HttpServletRequest, res: HttpServletResponse, chain: FilterChain) {
    if (req.getUserPrincipal != null) {
      throw new IllegalStateException(
        "Filter should not be used when other authentication is in place: " + req.getUserPrincipal.getClass.getName)
    }
    val request = getAuthenticatedUser(req) match {
      case None ⇒ req
      case Some(user) ⇒
        new HttpServletRequestWrapper(req) {
          var loggedOut = false
          override def isUserInRole(role: String) = !loggedOut && user.roles.contains(role)
          override def getUserPrincipal = if (loggedOut) null else user
          override def logout {
            logoutUser(req, res)
            super.logout()
            loggedOut = true
          }
          override def getAuthType = classOf[Authorization].getName
          override def getRemoteUser = if (loggedOut) null else user.getName
        }
    }
    chain.doFilter(request, res)
  }
}

/**
 * Apply this trait to an existing filter to get forwarding
 * (not redirect) to login page.
 */
trait AuthenticationForwarding extends Filter {

  import javax.activation.MimeType

  protected def loginPage: String
  private[this] val defaultAcceptTypes = Set("text/html").map(new MimeType(_))
  /** Accept types this filter applies to. Default is only "text/html". */
  protected def acceptTypes: Set[MimeType] = defaultAcceptTypes

  abstract override def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain) {
    super.doFilter(req, res, chain)
    (req, res) match {
      case (req: HttpServletRequest, res: HttpServletResponse) ⇒ httpFilter(req, res, chain)
      case _ ⇒ // Ignore
    }
  }

  private def httpFilter(req: HttpServletRequest, res: HttpServletResponse, chain: FilterChain) {
    if (!res.isCommitted) res.getStatus match {
      case SC_UNAUTHORIZED if AcceptHeader(req).forall(h ⇒ h.matchesAny(acceptTypes)) ⇒
        res.setStatus(SC_FOUND)
        req.getRequestDispatcher(loginPage).forward(req, res)
      case SC_FORBIDDEN ⇒ res.sendError(SC_FORBIDDEN)
      case _ ⇒ // Ignore anything else
    }
  }

}

/**
 * Stand-alone filter of AuthenticationForwarding trait.
 * Does nothing else.
 */
abstract class AuthenticationForwardingFilter extends NoOpFilter with AuthenticationForwarding
