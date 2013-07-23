package scuff

import java.util.Date

/**
 * Immutable extension of [[java.util.Date]].
 * Throws [[UnsupportedOperationException]] if accessing any mutating
 * method.
 */
final class Timestamp private (time: Long, toStr: String) extends Date(time) {

  def this() = this(System.currentTimeMillis, null)
  def this(time: Long) = this(time, null)
  def this(time: Date) = this(time.getTime, time.toString)

  override def toString = if (toStr != null) toStr else Timestamp.format(this)

  def asMillis = getTime

  /** Get Unix time, which is number of seconds since epoch. */
  def unixTime: Int = (getTime / 1000).asInstanceOf[Int]

  @deprecated(message = "Just say no to mutation", since = "1.2")
  @throws(classOf[UnsupportedOperationException])
  override def setTime(t: Long) = throw new UnsupportedOperationException

  @deprecated(message = "Just say no to mutation", since = "1.2")
  @throws(classOf[UnsupportedOperationException])
  override def setDate(d: Int) = throw new UnsupportedOperationException

  @deprecated(message = "Just say no to mutation", since = "1.2")
  @throws(classOf[UnsupportedOperationException])
  override def setHours(h: Int) = throw new UnsupportedOperationException

  @deprecated(message = "Just say no to mutation", since = "1.2")
  @throws(classOf[UnsupportedOperationException])
  override def setMinutes(m: Int) = throw new UnsupportedOperationException

  @deprecated(message = "Just say no to mutation", since = "1.2")
  @throws(classOf[UnsupportedOperationException])
  override def setMonth(m: Int) = throw new UnsupportedOperationException

  @deprecated(message = "Just say no to mutation", since = "1.2")
  @throws(classOf[UnsupportedOperationException])
  override def setSeconds(s: Int) = throw new UnsupportedOperationException

  @deprecated(message = "Just say no to mutation", since = "1.2")
  @throws(classOf[UnsupportedOperationException])
  override def setYear(y: Int) = throw new UnsupportedOperationException
}

object Timestamp {
  import scala.util.Try
  private[this] final val spinLock = new SpinLock
  private[this] final val Formatter = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
  private def format(time: Timestamp): String = spinLock.whenLocked(Formatter.format(time))
  def parseISO(str: String): Try[Timestamp] = Try {
    val date = spinLock.whenLocked(Formatter.parse(str))
    new Timestamp(date.getTime, str)
  }
}
