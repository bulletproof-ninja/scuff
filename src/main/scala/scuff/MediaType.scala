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
}

/** Immutable media type. */
class MediaType private (private val mimeType: MimeType) {

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

  def q: Float = mimeType.getParameter("q") match {
    case null => 1f
    case q => try q.toFloat catch {
      case nfe: NumberFormatException => 0f
    }
  }

  /** Matches the base type, ignores parameters. */
  def matches(string: String): Boolean = this.mimeType.`match`(string)
  /** Matches the base type, ignores parameters. */
  def matches(that: MediaType): Boolean = this.mimeType.`match`(that.mimeType)

  override def toString = mimeType.toString
}
