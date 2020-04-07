package scuff.web

import scala.util.Try
import scuff.MediaType
import collection.immutable.Seq
import collection.compat._

final class AcceptHeader(acceptTypes: Seq[MediaType]) {
  require(acceptTypes.nonEmpty, "Cannot have an empty Accept header")

  private def matches(specific: MediaType): Boolean = {
    val pruned = specific.pruned
    acceptTypes.exists { mt =>
      mt.primaryType == specific.primaryType &&
        (mt.subType == "*" || mt.subType == specific.subType || mt.subType == pruned.subType)
    }
  }

  def preference(): MediaType = preferenceOrdered.head
  def withParm(mt: MediaType, parmName: String): Iterable[(MediaType, String)] = withParm(mt, parmName, identity)
  def withParm[P](matchType: MediaType, parmName: String, map: String => P): Iterable[(MediaType, P)] =
    acceptTypes.iterator
      .filter(_.matches(matchType))
      .flatMap { mt =>
        mt.parm(parmName).flatMap(p => Try(map(p)).toOption.map(mt -> _))
      }.to(Iterable)
  def preferenceOrdered(): Seq[MediaType] = {
    if (acceptTypes.size == 1) acceptTypes else {
      val weigthed = acceptTypes.zipWithIndex.map {
        case (mt, idx) => (mt.q, mt, idx)
      }
      weigthed.sorted(AcceptHeader.Ordering).map(_._2)
    }
  }

  def hasExactly(contentType: String): Boolean = hasExactly(MediaType(contentType))
  def hasExactly(mediaType: MediaType): Boolean = {
    val primary = mediaType.primaryType
    val sub = mediaType.subType
    acceptTypes.exists { mt =>
      mt.primaryType == primary &&
        mt.subType == sub
    }
  }

  /** Accepts anything, i.e. is primary type `*`? */
  val acceptsAnything = acceptTypes.exists(mt => mt.primaryType == "*")
  /** Accepts media type? */
  def accepts(specific: String): Boolean = accepts(MediaType(specific))
  /** Accepts media type? */
  def accepts(specific: MediaType): Boolean = acceptsAnything || matches(specific)
  /** Accepts any of the media types? */
  def acceptsAnyOf(specifics: Iterable[MediaType]): Boolean = acceptsAnything || specifics.exists(matches)

  override def toString(): String = acceptTypes.mkString(", ")
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
    types.map(MediaType(_)).toList
  }
  def apply(header: String): Option[AcceptHeader] = Option(header).flatMap { header =>
    val types = split(header)
    if (types.isEmpty) None else Some(new AcceptHeader(types))
  }

  def apply(req: javax.servlet.http.HttpServletRequest): Option[AcceptHeader] = {
    import scala.jdk.CollectionConverters._
    val types = req.getHeaders("Accept").asScala.flatMap(split(_)).toList
    if (types.isEmpty) None else Some(new AcceptHeader(types))
  }
}
