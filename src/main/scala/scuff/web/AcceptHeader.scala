package scuff.web

import scala.util.Try

import javax.activation.MimeType

final class AcceptHeader(mimeTypes: Seq[MimeType]) {
  require(mimeTypes.nonEmpty, "Cannot have an empty Accept header")
  private[this] val hasMatchAny = mimeTypes.exists(mt => mt.getPrimaryType() == "*")
  private def matchesTypes(specific: MimeType) = mimeTypes.exists { mt =>
    mt.getPrimaryType == specific.getPrimaryType && (mt.getSubType == "*" || mt.getSubType == specific.getSubType)
  }

  def preference(): MimeType = preferenceOrdered.head
  def withParm(mt: MimeType, parm: String): Seq[(MimeType, String)] = withParm(mt, parm, identity)
  def withParm[P](matchType: MimeType, parm: String, map: String => P): Seq[(MimeType, P)] =
    mimeTypes.iterator
      .filter(_.`match`(matchType))
      .flatMap { mt =>
        mt.parm("v").flatMap(p => Try(map(p)).toOption.map(mt -> _))
      }.toStream
  def preferenceOrdered(): Seq[MimeType] = {
    if (mimeTypes.size == 1) mimeTypes else {
      val withQ = mimeTypes.zipWithIndex.map {
        case (mt, idx) => (mt.q, mt, idx)
      }
      withQ.sorted(AcceptHeader.MimeTypeOrdering).map(_._2)
    }
  }
  def matches(specific: String): Boolean = matches(new MimeType(specific))
  def matches(specific: MimeType): Boolean = hasMatchAny || matchesTypes(specific)
  def matchesAny(specifics: Traversable[MimeType]): Boolean = hasMatchAny || specifics.exists(matchesTypes)
}

object AcceptHeader {
  private type MTT = (Float, MimeType, Int)
  private val MimeTypeOrdering = new math.Ordering[MTT] {
    def compare(x: MTT, y: MTT): Int = {
      if (x._1 == y._1) {
        val xmt = x._2
        val ymt = y._2
        if (xmt.getPrimaryType == "*") 1
        else if (ymt.getPrimaryType == "*") -1
        else if (xmt.getPrimaryType == ymt.getPrimaryType) {
          if (xmt.getSubType == "*") 1
          else if (ymt.getSubType == "*") -1
          else if (xmt.getSubType == ymt.getSubType) {
            -xmt.toString.compareToIgnoreCase(ymt.toString)
          } else {
            x._3 - y._3
          }
        } else {
          -xmt.toString.compareToIgnoreCase(ymt.toString)
        }
      } else if (x._1 > y._1) -1
      else 1
    }
  }
  private val Splitter = """\s*,\s*""".r.pattern
  private def split(str: String): Seq[MimeType] = {
    val types = Splitter.split(str.trim).map(_.trim).filter(_.length > 0)
    types.map(new MimeType(_))
  }
  def apply(header: String): Option[AcceptHeader] = Option(header).flatMap { header =>
    val types = split(header)
    if (types.isEmpty) None else Some(new AcceptHeader(types))
  }

  def apply(req: javax.servlet.http.HttpServletRequest): Option[AcceptHeader] = {
    import collection.JavaConverters._
    val types = req.getHeaders("Accept").asScala.flatMap(split(_)).toSeq
    if (types.isEmpty) None else Some(new AcceptHeader(types))
  }
}
