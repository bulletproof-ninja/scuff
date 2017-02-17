package scuff

/**
  * Look up properties from generic source, with generic fallback.
  */
class Props(metaName: String, getProperty: String => String, fallback: Props = null) {
  def optional(name: String, validValues: Set[String] = Set.empty): Option[String] = {
    var value = getProperty(name)
    if (value == null && fallback != null) {
      value = fallback.optional(name, validValues).orNull
    }
    if (value == null) {
      None
    } else if (!validValues.isEmpty && !validValues.contains(value)) {
      throw new IllegalStateException(s"The $metaName '$name' has invalid value '$value'; valid values: [${validValues.mkString(", ")}]")
    } else {
      Some(value)
    }
  }

  @throws(classOf[IllegalStateException])
  def required(name: String, validValues: Set[String] = Set.empty): String = optional(name, validValues) match {
    case None if validValues.isEmpty => throw new IllegalStateException(s"Required $metaName '$name' missing")
    case None => throw new IllegalStateException(s"Required $metaName '$name' missing; valid values: [${validValues.mkString(", ")}]")
    case Some(value) => value
  }

}

class SysProps(fallback: Props) extends Props("system property", System.getProperty, fallback)
object SysProps extends SysProps(null)
class EnvVars(fallback: Props) extends Props("environment variable", System.getenv, fallback)
object EnvVars extends EnvVars(null)

object Props {
  import java.io._
  def apply(file: File, fallback: Props = null) = {
    require(file.exists, "File does not exist: " + file)
    require(file.isFile, "Not a file: " + file)
    require(file.canRead, "Cannot read: " + file)
    val props = new java.util.Properties
    props.load(new FileReader(file))
    new Props(s"${file.getName} property", props.getProperty, fallback)
  }
}
