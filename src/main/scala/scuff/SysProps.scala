package scuff

import scala.language.reflectiveCalls

/**
 * Look up environment variables with property override.
 */
class Props(props: { def getProperty(name: String): String }) {
  def optional(name: String, validValues: Set[String] = Set.empty): Option[String] = {
    var value = props.getProperty(name)
    if (value == null) {
      value = System.getenv(name)
    }
    if (value == null) {
      None
    } else if (!validValues.isEmpty && !validValues.contains(value)) {
      throw new IllegalStateException("Property '%s' value '%s' not among valid values: %s".format(name, value, validValues.mkString("[", ", ", "]")))
    } else {
      Some(value)
    }
  }

  @throws(classOf[IllegalStateException])
  def required(name: String, validValues: Set[String] = Set.empty): String = optional(name, validValues) match {
    case None if validValues.isEmpty => throw new IllegalStateException("Required system property missing: " + name)
    case None => throw new IllegalStateException("Required property missing: %s, valid values: %s".format(name, validValues.mkString("[", ", ", "]")))
    case Some(value) => value
  }

}

object SysProps extends Props(new { def getProperty(name: String) = System.getProperty(name) })

object Props {
  import java.io._
  def apply(file: File) = {
    require(file.exists, "File does not exist: " + file)
    require(file.isFile, "Not a file: " + file)
    require(file.canRead, "Cannot read: " + file)
    val props = new java.util.Properties
    props.load(new FileReader(file))
    new Props(props)
  }
}