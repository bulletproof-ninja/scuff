package scuff.web

import javax.servlet.Filter
import javax.servlet.FilterConfig

trait NoConfig { self: Filter =>
  final def init(config: FilterConfig) {}
  def destroy() {}
}