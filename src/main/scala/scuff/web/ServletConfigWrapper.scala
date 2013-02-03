package scuff.web

import javax.servlet.ServletConfig

class ServletConfigWrapper(config: ServletConfig) extends ServletConfig {
  def getInitParameter(parm: String) = config.getInitParameter(parm)
  def getInitParameterNames() = config.getInitParameterNames()
  def getServletContext() = config.getServletContext()
  def getServletName() = config.getServletName()
}
