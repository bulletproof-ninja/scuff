package scuff

import javax.activation.MimeType
import scala.collection.AbstractIterator

object MediaType {
  def apply(
    primaryType: String, subType: String,
    parms: (String, Any)*): MediaType = {
    val mt = parms.foldLeft(new MimeType(primaryType, subType)) {
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

  def apply(contentType: String): MediaType = new MediaType(new MimeType(contentType))

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
class MediaType private (private val mimeType: MimeType) {

  case class TreeType private[MediaType] (prefix: Option[String], typeName: String, suffixType: String) {
    def pruned: MediaType = {
      val mt = new MimeType(primaryType, suffixType)
      parmNames.foreach { name =>
        mt.setParameter(name, mimeType.getParameter(name))
      }
      new MediaType(mt)
    }
    override def toString() = MediaType.this.toString
  }

  def pruned: MediaType = treeType.map(_.pruned) getOrElse this
  def treeType: Option[TreeType] = MediaType.asTreeType(this)

  def baseType = mimeType.getBaseType
  def primaryType = mimeType.getPrimaryType
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

  def addParm(name: String, value: Any): MediaType = {
    val newMT = new MimeType(mimeType.getPrimaryType, mimeType.getSubType)
    parmNames.foreach { name =>
      newMT.setParameter(name, mimeType.getParameter(name))
    }
    newMT.setParameter(name, value.toString)
    new MediaType(newMT)
  }
  def removeParm(name: String): MediaType = {
    val newMT = new MimeType(mimeType.getPrimaryType, mimeType.getSubType)
    parmNames.filter(_ != name).foreach { name =>
      newMT.setParameter(name, mimeType.getParameter(name))
    }
    new MediaType(newMT)
  }

  def q: Float = mimeType.getParameter("q") match {
    case null => 1f
    case q => try q.toFloat catch {
      case nfe: NumberFormatException => 0f
    }
  }

  /** Matches the base type, ignores parameters. */
  def matches(string: String): Boolean = this.mimeType.`match`(string)
  /** Matches the base type, ignores parameters. */
  def matches(that: MediaType): Boolean =
    (this eq that) || this.mimeType.`match`(that.mimeType)

  override def toString = mimeType.toString
}
