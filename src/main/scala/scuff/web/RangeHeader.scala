package scuff.web

import scuff._
import collection.immutable.Seq

final case class RangeHeader(
    unit: String,
    ranges: Seq[RangeHeader.Range])

object RangeHeader {
  final case class Range(start: Long, end: Option[Long])(unit: String) {
    require(start >= 0 || end.isEmpty, s"""Invalid range: $start-${end.get}""")

    private def toRange(totalSize: Long): ContentRange.Range =
      if (start < 0) {
        val rangeStart = totalSize + start
        val rangeEnd = totalSize - 1
        ContentRange.Range(rangeStart, rangeEnd)
      } else {
        val rangeEnd = end || (totalSize - 1)
        ContentRange.Range(start, rangeEnd)
      }

    def length: Option[Long] = end.map(_ - start + 1)
    def lengthGivenSize(totalSize: Long): Long = toRange(totalSize).length

    def contentRange(totalSize: Long): ContentRange = contentRange(Some(totalSize))
    def contentRange(totalSize: Option[Long] = None): ContentRange = {
      ContentRange(unit, totalSize.map(toRange), totalSize)
    }
  }

  private val CommaSplitter = """\s*,\s*""".r.pattern
  private val UnitSplitter = """(\w+)=(.+)""".r
  private val RangePicker = """(-\d+)|(\d+)-(\d*)""".r

  private def split(unit: String, rangesStr: String): Seq[Range] = {
    CommaSplitter.split(rangesStr.trim).iterator
      .map(_.trim).filter(_.length > 0)
      .flatMap { range =>
        RangePicker.findFirstMatchIn(range).map { m =>
          m.group(1) match {
            case relative: String =>
              new Range(relative.toLong, None)(unit)
            case _ =>
              new Range(
                m.group(2).toLong,
                m.group(3).optional.map(_.toLong))(unit)
          }
        }
      }.toList
  }
  def apply(header: String): Option[RangeHeader] = Option(header).flatMap { header =>
    UnitSplitter.findFirstMatchIn(header.trim).flatMap { unitMatcher =>
      val unit = unitMatcher group 1
      val intervals = split(unit, unitMatcher group 2)
      if (intervals.isEmpty) None else Some(new RangeHeader(unit, intervals))
    }
  }

  def apply(req: javax.servlet.http.HttpServletRequest): Option[RangeHeader] =
    req.getHeader("Range").optional.flatMap(this.apply)

}
