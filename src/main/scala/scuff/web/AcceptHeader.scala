package scuff.web

import scala.util.Try
import scuff.MediaType

final class AcceptHeader(acceptTypes: Seq[MediaType]) {
  require(acceptTypes.nonEmpty, "Cannot have an empty Accept header")
  private[this] val hasMatchAny = acceptTypes.exists(mt => mt.primaryType == "*")
  private def matchesTypes(specific: MediaType) = {
    val pruned = specific.pruned
    acceptTypes.exists { mt =>
      mt.primaryType == specific.primaryType &&
        (mt.subType == "*" || mt.subType == specific.subType || mt.subType == pruned.subType)
    }
  }

  def preference(): MediaType = preferenceOrdered.head
  def withParm(mt: MediaType, parmName: String): Seq[(MediaType, String)] = withParm(mt, parmName, identity)
  def withParm[P](matchType: MediaType, parmName: String, map: String => P): Seq[(MediaType, P)] =
    acceptTypes.iterator
      .filter(_.matches(matchType))
      .flatMap { mt =>
        mt.parm(parmName).flatMap(p => Try(map(p)).toOption.map(mt -> _))
      }.toStream
  def preferenceOrdered(): Seq[MediaType] = {
    if (acceptTypes.size == 1) acceptTypes else {
      val weigthed = acceptTypes.zipWithIndex.map {
        case (mt, idx) => (mt.q, mt, idx)
      }
      weigthed.sorted(AcceptHeader.Ordering).map(_._2)
    }
  }
  def accepts(specific: String): Boolean = hasMatchAny || accepts(MediaType(specific))
  def accepts(specific: MediaType): Boolean = hasMatchAny || matchesTypes(specific)
  def acceptsAny(specifics: Traversable[MediaType]): Boolean = hasMatchAny || specifics.exists(matchesTypes)
}

object AcceptHeader {
  private type Weigthed = (Float, MediaType, Int)
  private val Ordering = new math.Ordering[Weigthed] {
    def compare(x: Weigthed, y: Weigthed): Int = {
      if (x._1 == y._1) {
        val xmt = x._2
        val ymt = y._2
        if (xmt.primaryType == "*") 1
        else if (ymt.primaryType == "*") -1
        else if (xmt.primaryType == ymt.primaryType) {
          if (xmt.subType == "*") 1
          else if (ymt.subType == "*") -1
          else if (xmt.subType == ymt.subType) {
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
  private def split(str: String): Seq[MediaType] = {
    val types = Splitter.split(str.trim).map(_.trim).filter(_.length > 0)
    types.map(MediaType(_))
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
