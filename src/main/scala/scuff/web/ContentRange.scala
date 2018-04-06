package scuff.web

import javax.servlet.http.HttpServletResponse

case class ContentRange(unit: String, range: Option[ContentRange.Range], size: Option[Long]) {

  def this(unit: String, start: Long, end: Long, size: Long) = this(unit, Some(ContentRange.Range(start, end)), Some(size))
  def this(unit: String, start: Long, end: Long) = this(unit, Some(ContentRange.Range(start, end)), None)
  def this(unit: String, size: Long) = this(unit, None, Some(size))

  def headerString = {
    val rangeStr = range.map(r => s"${r.start}-${r.end}") getOrElse "*"
    s"$unit $rangeStr/${size getOrElse "*"}"
  }

  def setTo(res: HttpServletResponse) = res.setHeader(HttpHeaders.ContentRange, headerString)
  def addTo(res: HttpServletResponse) = res.addHeader(HttpHeaders.ContentRange, headerString)

  def contentLength: Option[Long] = range.map(r => (r.end - r.start) + 1)

  override def toString() = s"${HttpHeaders.ContentRange}: $headerString"
}

object ContentRange {
  final case class Range(start: Long, end: Long) {
    require(start >= 0, s"Start must be non-negative: $start")
    require(end >= 0 && end >= start, s"End must be non-negative and not less than start($start): $end")
    def length: Long = end - start + 1
  }
}
