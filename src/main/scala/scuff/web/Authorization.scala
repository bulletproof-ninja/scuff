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

  private[this] var filterName: String = _
  def init(config: FilterConfig) {
    filterName = Option(config.getFilterName).getOrElse(getClass.getName)
  }

  def destroy {}

  def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain) = (req, res) match {
    case (req: HttpServletRequest, res: HttpServletResponse) ⇒ httpFilter(req, res, chain)
    case _ ⇒ chain.doFilter(req, res)
  }

  private def getRequest(req: HttpServletRequest, res: HttpServletResponse): HttpServletRequest = {
    getAuthenticatedUser(req) match {
      case Some(authUser) ⇒
        new HttpServletRequestWrapper(req) {
          @volatile var user: Option[UserPrincipal] = Some(authUser)
          override def isUserInRole(role: String) = user.exists(_.roles.contains(role))
          override def getUserPrincipal = user.orNull
          override def logout {
            logoutUser(req, res)
            super.logout()
            user = None
          }
          override def getAuthType = classOf[Authorization].getName
          override def getRemoteUser = user.map(_.getName).orNull
        }
      case _ ⇒ req
    }
  }

  private def httpFilter(req: HttpServletRequest, res: HttpServletResponse, chain: FilterChain) {
    req.getUserPrincipal match {
      case null ⇒ chain.doFilter(getRequest(req, res), res)
      case _: UserPrincipal ⇒
        throw new IllegalStateException(
          "%s filter loop detected.".format(filterName))
      case p: Principal ⇒
        throw new IllegalStateException(
          "%s filter should not be used when other authentication is in place: %s".format(filterName, p.getClass.getName))
    }
  }
}

/**
 * Apply this trait to an existing filter to get forwarding
 * (not redirect) to login page.
 */
trait LoginPageForwarder extends Filter {

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
      case SC_UNAUTHORIZED ⇒
        if (req.getMethod().equalsIgnoreCase("GET") && AcceptHeader(req).forall(_.matchesAny(acceptTypes))) {
          res.setStatus(SC_FOUND)
          req.getRequestDispatcher(loginPage).forward(req, res)
        } else {
          res.sendError(SC_UNAUTHORIZED)
        }
      case SC_FORBIDDEN ⇒
        res.sendError(SC_FORBIDDEN)
      case _ ⇒ // Ignore anything else
    }
  }

}

/**
 * Stand-alone filter of AuthenticationForwarding trait.
 * Does nothing else.
 */
abstract class LoginPageForwardingFilter extends NoOpFilter with LoginPageForwarder
