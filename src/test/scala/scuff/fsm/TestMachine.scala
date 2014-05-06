package scuff.fsm

import org.junit._
import org.junit.Assert._
import language.reflectiveCalls
import scala.util.Try

class TestFSM {
  @Test
  def connection {
    import ConnectionEvents._
    val conn = new Connection
    import conn.{ Active, Disabled, Forgotten }
    assertFalse(conn.isFinal)
    assertFalse(conn is Forgotten)
    assertFalse(conn is Active)
    assertFalse(conn is Disabled)
    conn.start(enabled = true)
    assertTrue(conn is Active)
    conn(disable)
    assertFalse(conn is Active)
    assertTrue(conn is Disabled)
    try {
      conn(reconnected)
      fail("Cannot reconnect when already connected")
    } catch {
      case e: IllegalStateException ⇒ // Expected
    }
    conn(disconnected)
    assertFalse(conn.isFinal)
    conn(forget)
    assertTrue(conn.isFinal)
    assertTrue(conn is Forgotten)
  }
  @Test
  def dateParser {
    val parser = new ISODateParser
    import parser.{ dash, digit, done }
    assertEquals(None, parser.current)
    parser.start()
    val isoDate = "2012-12-31"
    for (c ← isoDate) c match {
      case '-' ⇒ parser(dash)
      case c if c >= '0' && c <= '9' ⇒ parser(digit, c)
    }
    parser(done)
    parser.current match {
      case Some(comp @ parser.Completed) ⇒ comp.date match {
        case Some(date) ⇒ assertEquals(isoDate, date.toString)
        case _ ⇒ fail("Should have some date")
      }
      case _ ⇒ fail("current state should be completed")
    }
  }

  object ConnectionEvents extends Enumeration {
    val enable, disable, reconnected, disconnected, forget, silence = new Val() with Event
  }

  class Connection extends FSM {

    def start(enabled: Boolean) = {
      val initial = if (enabled) Active.Connected else Disabled.Connected
      super.init(initial)
    }

    val Active = new SuperState {
      val Connected = new State
      val Disconnected = new SuperState {
        val Alarm = new State
        val Silent = new State
      }
    }
    val Disabled = new SuperState {
      val Connected = new State
      val Disconnected = new State
    }
    val Forgotten = new FinalState

    import ConnectionEvents._
    val transitions: Set[Transition] = Set(
      Active.Connected -> disable -> Disabled.Connected,
      Active.Connected -> disconnected -> Active.Disconnected.Alarm,
      Active.Disconnected.Alarm -> silence -> Active.Disconnected.Silent,
      Active.Disconnected -> reconnected -> Active.Connected,
      Active.Disconnected -> forget -> Forgotten,

      Disabled.Connected -> disconnected -> Disabled.Disconnected,
      Disabled.Disconnected -> reconnected -> Disabled.Connected,
      Disabled.Disconnected -> forget -> Forgotten)

    def isDisconnected = is(Disabled.Disconnected) || is(Active.Disconnected)
  }

  class ISODateParser extends typed.FSM[Char] {

    def start() {
      super.init(ParseYear)
    }

    object digit extends Event
    object dash extends Event
    object done extends Event

    sealed class ParseState extends typed.State[Char] {
      var num = 0
      override def onEvent(evt: Event, char: Char) = evt match {
        case `digit` ⇒
          onDigit(char)
          evt
        case _ ⇒ evt
      }
      def onDigit(char: Char) = num = (num * 10) + (char - '0')
    }
    val ParseYear = new ParseState
    val ParseMonth = new ParseState
    val ParseDay = new ParseState
    val Completed = new FinalState {
      var date: Option[java.sql.Date] = None
      override def onEvent(evt: Event) = evt match {
        case `done` ⇒ date = Some(new java.sql.Date(ParseYear.num - 1900, ParseMonth.num - 1, ParseDay.num))
      }
    }

    val transitions: Set[Transition] = Set(
      ParseYear -> digit -> ParseYear,
      ParseYear -> dash -> ParseMonth,
      ParseMonth -> digit -> ParseMonth,
      ParseMonth -> dash -> ParseDay,
      ParseDay -> digit -> ParseDay,
      ParseDay -> done -> Completed)
  }
  @Test
  def `phone, invalid number` {
    import OldSchoolPhoneEvents._
    val phone = new OldSchoolPhone
    import phone._
    phone.plugIn()
    assertTrue(phone is Idle)
    assertTrue(Try(phone(TimedOut)).isFailure)
    phone(ReceiverLifted)
    assertTrue(phone is Active)
    phone(HungUp)
    assertFalse(phone is Active)
    assertTrue(phone is Idle)
    phone(DigitDialed, '0')
    assertTrue(phone is Idle)
    phone(ReceiverLifted)
    phone(DigitDialed, '0')
    assertTrue(phone is Active.InvalidNumber)
  }
  @Test
  def `phone, busy, try again, talk` {
    val number = "234-235-5678".filterNot(_ == '-')
    import OldSchoolPhoneEvents._
    val phone = new OldSchoolPhone
    import phone._
    phone.plugIn()
    phone(ReceiverLifted)
    assertTrue(phone is Active.DialTone)
    val (first, rest) = number.head -> number.tail
    phone(DigitDialed, first)
    rest.foreach { digit ⇒
      assertTrue(phone is Active.Dialing)
      phone(DigitDialed, digit)
    }
    assertTrue(phone is Active.Connecting)
    phone(NumberBusy)
    assertTrue(phone is Active.Busy)
    phone(HungUp)
    assertTrue(phone is Idle)

    phone(ReceiverLifted)
    assertTrue(phone is Active.DialTone)
    number.foreach(phone(DigitDialed, _))
    assertTrue(phone is Active.Connecting)
    phone(Connected)
    assertTrue(phone is Active.Ringing)
    phone(HungUp)
    assertTrue(phone is Idle)

    phone(ReceiverLifted)
    number.foreach(phone(DigitDialed, _))
    assertEquals(number, Active.number.result)
    phone(Connected)
    assertTrue(phone is Active.Ringing)
    phone(CalleeReceiverLifted)
    assertTrue(phone is Active.Talking)
  }

}
object OldSchoolPhoneEvents {
  case object HungUp extends Event
  case object ReceiverLifted extends Event
  case object TimedOut extends Event
  case object NumberBusy extends Event
  case object Connected extends Event
  case object CalleeReceiverLifted extends Event
  case object CalleeHungUp extends Event
  case object DigitDialed extends Event
  case class DigitDialed(valid: Boolean = true, complete: Boolean = false) extends Event
}
import OldSchoolPhoneEvents._
class OldSchoolPhone extends FSM {
  private def toInt(char: Char) = char - '0'
  def plugIn() = super.init(Idle)
  val Idle = new State
  val Active = new SuperState {
    val number = new StringBuilder
    val DialTone = new State {
      override def onEvent(evt: Event, payload: Any): Event = evt match {
        case DigitDialed ⇒
          val digit = toInt(payload.asInstanceOf[Char])
          DigitDialed(valid = (2 <= digit && digit <= 9))
        case ReceiverLifted ⇒
          number.clear()
          evt
        case _ ⇒ evt
      }
    }
    val Dialing = new State {
      override def onEvent(evt: Event, payload: Any): Event = evt match {
        case DigitDialed ⇒
          val digit = toInt(payload.asInstanceOf[Char])
          onEvent(DigitDialed(0 <= digit && digit <= 9), payload)
        case DigitDialed(true, _) ⇒
          number += payload.asInstanceOf[Char]
          DigitDialed(true, number.length == 10)
      }
    }
    val Connecting, Timeout, Busy, Ringing, Talking, Pinned, InvalidNumber = new State
  }

  import OldSchoolPhoneEvents._
  import Active._
  val transitions: Set[Transition] = Set(
    Idle -> DigitDialed -> Idle,
    Idle -> ReceiverLifted -> DialTone,
    Active -> HungUp -> Idle,

    DialTone -> DigitDialed(valid = true) -> Dialing,
    DialTone -> TimedOut -> Timeout,
    DialTone -> DigitDialed(valid = false) -> InvalidNumber,

    Dialing -> DigitDialed(valid = true) -> Dialing,
    Dialing -> TimedOut -> Timeout,
    Dialing -> DigitDialed(valid = false) -> InvalidNumber,
    Dialing -> DigitDialed(complete = true) -> Connecting,

    Connecting -> NumberBusy -> Busy,
    Connecting -> Connected -> Ringing,

    Ringing -> CalleeReceiverLifted -> Talking,

    Talking -> CalleeHungUp -> Pinned,
    Pinned -> CalleeReceiverLifted -> Talking)
}
