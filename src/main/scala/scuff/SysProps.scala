package scuff

/**
  * Look up properties from generic source, with generic fallback.
  */
class Props(getProperty: String => String, fallback: Props = null) {
  def optional(name: String, validValues: Set[String] = Set.empty): Option[String] = {
    var value = getProperty(name)
    if (value == null && fallback != null) {
      value = fallback.optional(name).orNull
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

class SysProps(fallback: Props) extends Props(System.getProperty, fallback)
object SysProps extends SysProps(null)
class EnvVars(fallback: Props) extends Props(System.getenv, fallback)
object EnvVars extends EnvVars(null)

object Props {
  import java.io._
  def apply(file: File, fallback: Props = null) = {
    require(file.exists, "File does not exist: " + file)
    require(file.isFile, "Not a file: " + file)
    require(file.canRead, "Cannot read: " + file)
    val props = new java.util.Properties
    props.load(new FileReader(file))
    new Props(props.getProperty, fallback)
  }
}
