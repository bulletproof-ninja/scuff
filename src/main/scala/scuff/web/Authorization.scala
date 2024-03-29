package scuff.web

import scuff.MediaType
import scuff.UserPrincipal

import java.security.Principal
import javax.servlet._
import javax.servlet.http._
import scala.annotation.nowarn

import HttpServletResponse._

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

  override def service(req: HttpServletRequest, res: HttpServletResponse): Unit = {
    req.getUserPrincipal match {
      case null => res.setStatus(SC_UNAUTHORIZED)
      case _ =>
        val allowed = rolesAllowed
        if (allowed.isEmpty || allowed.exists(req.isUserInRole)) {
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
  protected def getAuthenticatedUser(implicit req: HttpServletRequest, res: HttpServletResponse): Option[UserPrincipal]
  protected def logoutUser(implicit req: HttpServletRequest, res: HttpServletResponse): Unit

  def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain): Unit = httpFilter(req, res, chain)

  private def getRequest(req: HttpServletRequest, res: HttpServletResponse): HttpServletRequest = {
    getAuthenticatedUser(req, res) match {
      case Some(authUser) =>
        new HttpServletRequestProxy(req) {
          @volatile var user: Option[UserPrincipal] = Some(authUser)
          override def isUserInRole(role: String) = user.exists(_.roles.contains(role))
          override def getUserPrincipal = user.orNull
          override def logout(): Unit = {
            logoutUser(req, res)
            super.logout()
            user = None
          }
          override def getAuthType = classOf[Authorization].getName
          override def getRemoteUser = user.map(_.getName).orNull
        }
      case _ => req
    }
  }

  @inline
  private def httpFilter(req: HttpServletRequest, res: HttpServletResponse, chain: FilterChain): Unit = {
    req.getUserPrincipal match {
      case null => chain.doFilter(getRequest(req, res), res)
      case _: UserPrincipal =>
        throw new IllegalStateException(
          s"${getClass.getName} filter loop detected.")
      case p: Principal =>
        throw new IllegalStateException(
          s"${getClass.getName} filter should not be used when other authentication is in place: ${p.getClass.getName}")
    }
  }
}

/**
 * Apply this trait to an existing filter to get forwarding
 * (not redirect) to login page.
 */
trait LoginPageForwarder extends Filter {

  /** Login page path. */
  protected def loginPage: String

  private[this] val defaultAcceptTypes = Set("text/html").map(MediaType(_))
  /** Accept types this filter applies to. Default is only "text/html". */
  protected def acceptTypes: Set[MediaType] = defaultAcceptTypes

  abstract override def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain): Unit = {
    super.doFilter(req, res, chain)
    httpFilter(req, res, chain)
  }

  @inline
  private def httpFilter(req: HttpServletRequest, res: HttpServletResponse, @nowarn chain: FilterChain): Unit = {
    if (!res.isCommitted) res.getStatus match {
      case SC_UNAUTHORIZED =>
        if (req.getMethod().equalsIgnoreCase("GET") && AcceptHeader(req).forall(_.acceptsAnyOf(acceptTypes))) {
          res.setStatus(SC_FOUND)
          req.getRequestDispatcher(loginPage).forward(req, res)
        } else {
          res.sendError(SC_UNAUTHORIZED)
        }
      case SC_FORBIDDEN =>
        res.sendError(SC_FORBIDDEN)
      case _ => // Ignore anything else
    }
  }

}

/**
 * Stand-alone filter of AuthenticationForwarding trait.
 * Does nothing else.
 */
abstract class LoginPageForwardingFilter extends NoOpFilter with LoginPageForwarder
