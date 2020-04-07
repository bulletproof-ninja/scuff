package scuff

import Props.Key

import java.util.jar.Attributes
import java.util.jar.Manifest
import java.util.jar.JarFile

import scala.util.control.NonFatal
import scala.reflect.{ ClassTag, classTag }

/**
 * Look up properties from generic source, with generic fallback.
 */
class Props protected (metaName: String, getProperty: String => String, fallback: Props = null) {
  def optional(name: String): Option[String] =
    optional(new Key(name)(identity))
  def optional(name: String, validValues: Set[String]): Option[String] =
    optional(new Key(name, validValues.toSeq: _*)(identity))
  def optional[T](key: Key[T]): Option[T] = {
    getProperty(key.name) match {
      case null if fallback != null => fallback.optional(key)
      case null => None
      case value =>
        val typedValue = key.typed(value)
        if (!key.validValues.isEmpty && !key.validValues.contains(typedValue)) {
          throw new IllegalStateException(s"The $metaName '${key.name}' has invalid value '$value'; valid values: [${key.validValues.mkString(", ")}]")
        } else {
          Some(typedValue)
        }
    }
  }

  def required(name: String): String =
    required(new Key(name)(identity))
  def required(name: String, validValues: Set[String]): String =
    required(new Key(name, validValues.toSeq: _*)(identity))
  @throws(classOf[IllegalStateException])
  def required[T](key: Key[T]): T = optional(key) match {
    case None if key.validValues.isEmpty => throw new IllegalStateException(s"Required $metaName '${key.name}' missing")
    case None => throw new IllegalStateException(s"Required $metaName '${key.name}' missing; valid values: [${key.validValues.mkString(", ")}]")
    case Some(value) => value
  }

}

class SysProps(fallback: Props) extends Props("system property", System.getProperty, fallback)
object SysProps extends SysProps(null) with PropsKey

class EnvVars(fallback: Props) extends Props("environment variable", System.getenv, fallback)
object EnvVars extends EnvVars(null) with PropsKey
class ManifestAttributes(attrs: Attributes, fallback: Props) extends Props("manifest attribute", attrs.getValue, fallback) {
  def this(manifest: Manifest, fallback: Props) = this(manifest.getMainAttributes, fallback)
}

object ManifestAttributes extends AnyRef with PropsKey {
  import scala.jdk.CollectionConverters._
  def apply(cl: ClassLoader = getClass.getClassLoader, fallback: Props = null): Option[Props] = {
    cl.getResources(JarFile.MANIFEST_NAME).asScala.foldLeft(None: Option[ManifestAttributes]) {
      case (chain, url) => Some {
        val stream = url.openStream()
        try {
          val manifest = new Manifest(stream)
          new ManifestAttributes(manifest, chain orElse Option(fallback) orNull)
        } finally stream.close()
      }
    }
  }
}

trait PropsKey {
  def Key[T: ClassTag](name: String, validValues: T*)(implicit typed: String => T): Key[T] =
    Props.Key(name, validValues: _*)(typed)
  def Key[T: ClassTag](name: String, typed: String => T, validValues: T*): Key[T] =
    Props.Key(name, validValues: _*)(typed)
}

object Props {
  import java.io._

  case class Key[+T: ClassTag](name: String, validValues: T*)(_typed: String => T) {
    private[Props] def typed(value: String): T = try _typed(value) catch {
      case NonFatal(th) => throw new IllegalArgumentException(
          s"""Key "$name" value "$value" does not conform to ${classTag[T].runtimeClass}""", th)
    }
    def typeName = classTag[T].runtimeClass.getSimpleName
  }

  def apply(description: String, values: (String, String)*): Props = apply(description, values.toMap)
  def apply(description: String, values: collection.Map[String, String]): Props = apply(description, values, null)
  def apply(description: String, values: collection.Map[String, String], fallback: Props): Props =
    new Props(description, values.apply, fallback)

  def apply(description: String, reader: Reader): Props = apply(description, reader, null)
  def apply(description: String, reader: Reader, fallback: Props) = {
    val props = new java.util.Properties
    try {
      props.load(reader)
      new Props(s"$description property", props.getProperty, fallback)
    } finally {
      reader.close()
    }
  }

  def apply(file: File): Props = apply(file, null)
  def apply(file: File, fallback: Props): Props = {
    require(file.exists, "Must exist: " + file)
    require(file.isFile, "Must be a file: " + file)
    require(file.canRead, "Must be readable: " + file)
    apply(file.getName, new FileReader(file), fallback)
  }

  def apply(description: String, inp: InputStream): Props = apply(description, inp, null)
  def apply(description: String, inp: InputStream, fallback: Props): Props = {
    require(inp != null, "Input stream cannot be null.")
    apply(description, new InputStreamReader(inp), fallback)
  }

  def resource(cls: Class[_]): Props = resource(cls, null)
  def resource(cls: Class[_], fallback: Props): Props = {
    val resourceName = "/" + cls.getName.replace('.', '/') + ".properties"
    val inp = cls.getResourceAsStream(resourceName).ensuring(_ != null, s"Classpath resource not found: $resourceName")
    apply(s"$resourceName property", inp, fallback)
  }
}
