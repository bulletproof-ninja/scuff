package scuff.web

import scala.util.Try
import javax.activation.MimeType

final class AcceptHeader(mimeTypes: Seq[MimeType]) {
  require(!mimeTypes.isEmpty, "Cannot have an empty Accept header")
  private[this] val hasMatchAny = mimeTypes.exists(mt ⇒ mt.getPrimaryType() == "*")
  private def matchesTypes(specific: MimeType) = mimeTypes.exists { mt ⇒
    mt.getPrimaryType == specific.getPrimaryType && (mt.getSubType == "*" || mt.getSubType == specific.getSubType)
  }
  def preference(): MimeType = preferenceOrderd().head
  def preferenceOrderd(): Seq[MimeType] = {
    if (mimeTypes.size == 1) mimeTypes else {
      val withQ = mimeTypes.zipWithIndex.map {
        case (mt, idx) ⇒
          val q = mt.getParameter("q") match {
            case null ⇒ 1f
            case q ⇒ Try(q.toFloat).getOrElse(0f)
          }
          (q, mt, idx)
      }
      withQ.sorted(AcceptHeader.MimeTypeOrdering).map(_._2)
    }
  }
  def matches(specific: String): Boolean = hasMatchAny || matchesTypes(new MimeType(specific))
  def matches(specific: MimeType): Boolean = hasMatchAny || matchesTypes(specific)
  def matchesAny(specifics: Traversable[MimeType]) = hasMatchAny || specifics.exists(matchesTypes)
      }

object AcceptHeader {
  private type MTT = (Float, MimeType, Int)
  private val MimeTypeOrdering = new math.Ordering[MTT] {
    def compare(x: MTT, y: MTT): Int = {
      if (x._1 == y._1) {
        val xmt = x._2
        val ymt = y._2
        if (xmt.getPrimaryType == "*") {
          1
        } else if (ymt.getPrimaryType == "*") {
          -1
        } else if (xmt.getPrimaryType == ymt.getPrimaryType) {
          if (xmt.getSubType == "*") {
            1
          } else if (ymt.getSubType == "*") {
            -1
          } else if (xmt.getSubType == ymt.getSubType) {
            -xmt.toString.compareToIgnoreCase(ymt.toString)
          } else {
            x._3 - y._3
          }
        } else {
          -xmt.toString.compareToIgnoreCase(ymt.toString)
        }
      } else if (x._1 > y._1) {
        -1
      } else {
        1
  }
    }
  }
  private val Splitter = """\s*,\s*""".r.pattern
  private val EmptyArray = new Array[MimeType](0)
  private def split(str: String): Seq[MimeType] = {
    val types = Splitter.split(str.trim)
    types.length match {
      case 0 ⇒ Seq.empty
      case 1 if types(0).length == 0 ⇒ EmptyArray
      case _ ⇒ types.map(new MimeType(_))
    }
  }
  def apply(header: String): Option[AcceptHeader] = Option(header).flatMap { header ⇒
    val types = split(header)
    if (types.isEmpty) None else Some(new AcceptHeader(types))
}

  def apply(req: javax.servlet.http.HttpServletRequest): Option[AcceptHeader] = {
    import collection.JavaConverters._
    val types = req.getHeaders("Accept").asScala.flatMap(split(_)).toSeq
    if (types.isEmpty) None else Some(new AcceptHeader(types))
  }
}
