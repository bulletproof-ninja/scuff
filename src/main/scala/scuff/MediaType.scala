package scuff

import scala.collection.AbstractIterator
import scala.util.{ Failure, Try }

import javax.activation._
import scala.reflect.{ ClassTag, classTag }
import scala.util.control.NonFatal

object MediaType {

  @inline
  private def invalidMimeType(mt: String, cause: MimeTypeParseException) =
    new IllegalArgumentException(s"Invalid media type: $mt", cause)

  @inline
  private def newMimeType(mt: String): MimeType =
    try new MimeType(mt) catch {
      case e: MimeTypeParseException => throw invalidMimeType(mt, e)
    }
  @inline
  private def newMimeType(p: String, s: String): MimeType =
    try new MimeType(p, s) catch {
      case e: MimeTypeParseException => throw invalidMimeType(s"$p/$s", e)
    }
  def apply(
      primaryType: String, subType: String,
      parms: (String, Any)*): MediaType = {
    val mt = parms.foldLeft(newMimeType(primaryType, subType)) {
      case (mt, parm) =>
        mt.setParameter(parm._1, parm._2.toString)
        mt
    }
    new MediaType(mt)
  }
  def apply(
      primaryType: String, subType: String,
      parms: collection.Map[String, Any]): MediaType =
    apply(primaryType, subType, parms.toSeq: _*)

  def apply(contentType: String): MediaType = new MediaType(newMimeType(contentType))

  private val FindTreeSuffix = """\+(.+)$""".r
  private def asTreeType(mediaType: MediaType): Option[mediaType.TreeType] = {
    FindTreeSuffix.findFirstMatchIn(mediaType.subType).flatMap(m => Option(m.group(1))).map { suffix =>
      val subType = mediaType.subType
      val (prefix, typeName) = subType.indexOf('.') match {
        case -1 =>
          None ->
            subType.substring(0, subType.length - suffix.length - 1)
        case dot =>
          Some(subType.substring(0, dot)) ->
            subType.substring(dot + 1, subType.length - suffix.length - 1)
      }
      new mediaType.TreeType(prefix, typeName, suffix)
    }
  }
}

/** Immutable media type. */
class MediaType private (private val mimeType: MimeType) extends Serializable {

  case class TreeType private[MediaType] (prefix: Option[String], typeName: String, suffixType: String) {
    def pruned: MediaType = {
      val mt = MediaType.newMimeType(primaryType, suffixType)
      parmNames.foreach { name =>
        mt.setParameter(name, mimeType.getParameter(name))
      }
      new MediaType(mt)
    }
    override def toString() = MediaType.this.toString
  }

  /**
   * If tree type, prunes the media type down to the base type,
   * with parameters.
   * E.g. `application/vnd.foo.bar+json;q=0.8` becomes
   * `application/json;q=0.8`.
   * To get a clean base type, use `mediaType.pruned.baseType`.
   */
  def pruned: MediaType = treeType.map(_.pruned) getOrElse this

  /** As tree type, if defined. */
  def treeType: Option[TreeType] = MediaType.asTreeType(this)

  /** The base type, without parameters. */
  def baseType = mimeType.getBaseType
  /** The primary type, i.e. `primary/sub`. */
  def primaryType = mimeType.getPrimaryType
  /** The sub type, i.e. `primary/sub`. */
  def subType = mimeType.getSubType

  def parmNames: Iterator[String] = {
    if (mimeType.getParameters.isEmpty) Iterator.empty
    else new AbstractIterator[String] {
      private[this] val names = mimeType.getParameters.getNames
      def hasNext = names.hasMoreElements
      def next = names.nextElement.toString
    }
  }

  def parm(name: String): Option[String] = Option(mimeType.getParameter(name))

  def parm[T: ClassTag](name: String, map: String => T): Try[T] = mimeType.getParameter(name) match {
    case null => Failure(new NoSuchElementException(s"""Parameter "$name" not found"""))
    case str => Try(map(str)).recover {
      case NonFatal(th) =>
        throw new IllegalArgumentException(
          s"""Parameter "$name" not of type ${classTag[T].runtimeClass.getSimpleName}: $str""", th)
    }
  }

  lazy val parms: Map[String, String] = parmNames.foldLeft(Map.empty[String, String]) {
    case (map, name) => map.updated(name, mimeType.getParameter(name))
  }

  def addParm(name: String, value: Any): MediaType = {
    val newMT = MediaType.newMimeType(mimeType.getPrimaryType, mimeType.getSubType)
    parmNames.foreach { name =>
      newMT.setParameter(name, mimeType.getParameter(name))
    }
    newMT.setParameter(name, value.toString)
    new MediaType(newMT)
  }
  def removeParm(name: String): MediaType = {
    if (mimeType.getParameters.isEmpty || mimeType.getParameter(name) == null) this
    else {
      val newMT = MediaType.newMimeType(mimeType.getPrimaryType, mimeType.getSubType)
      parmNames.filter(_ != name).foreach { name =>
        newMT.setParameter(name, mimeType.getParameter(name))
      }
      new MediaType(newMT)
    }
  }

  def q: Float = mimeType.getParameter("q") match {
    case null => 1f
    case q => try q.toFloat catch {
      case _: NumberFormatException => 0f
    }
  }

  /** Matches the base type, ignores parameters. */
  def matches(string: String): Boolean = this.mimeType.`match`(string)
  /** Matches the base type, ignores parameters. */
  def matches(that: MediaType): Boolean =
    (this eq that) || this.mimeType.`match`(that.mimeType)

  override lazy val hashCode = java.util.Arrays hashCode Array(primaryType.##, subType.##, parms.##)
  override def equals(any: Any): Boolean = any match {
    case that: MediaType => (this eq that) || {
      this.primaryType == that.primaryType &&
      this.subType == that.subType &&
      this.parms == that.parms
    }
    case _ => false
  }

  override def toString = mimeType.toString
}
