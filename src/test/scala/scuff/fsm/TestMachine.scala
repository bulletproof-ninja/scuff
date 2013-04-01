package scuff.fsm

import org.junit._
import org.junit.Assert._

class TestFSM {
  @Test
  def connection {
    val conn = new Connection
    import conn.{ Active, Disabled, Forgotten }
    assertFalse(conn.isFinal)
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

case object enable extends Event
case object disable extends Event
case object reconnected extends Event
case object disconnected extends Event
case object forget extends Event
case object silence extends Event

class Connection extends FSM {

  def start(enabled: Boolean) = {
    val initial = if (enabled) Active.Connected else Disabled.Connected
    super.start(initial)
  }

  val Active = new SuperState {
    val Connected = new LeafState
    val Disconnected = new SuperState {
      val Alarm = new LeafState
      val Silent = new LeafState
    }
  }
  val Disabled = new SuperState {
    val Connected = new LeafState
    val Disconnected = new LeafState
  }
  val Forgotten = new FinalState

  val transitions: Set[Transition] = Set(
    Active.Connected -> disable -> Disabled.Connected,
    Active.Connected -> disconnected -> Active.Disconnected.Alarm,
    Active.Disconnected.Alarm -> silence -> Active.Disconnected.Silent,
    Active.Disconnected -> reconnected -> Active.Connected,
    Active.Disconnected -> forget -> Forgotten,

    Disabled.Connected -> disconnected -> Disabled.Disconnected,
    Disabled.Disconnected -> reconnected -> Disabled.Connected,
    Disabled.Disconnected -> forget -> Forgotten
  )

  def isDisconnected = is(Disabled.Disconnected) || is(Active.Disconnected)
}

class ISODateParser extends FSM {

  def start() {
    super.start(ParseYear)
  }

  object digit extends Event
  object dash extends Event
  object done extends Event

  sealed class ParseState extends LeafState {
    var num = 0
      override def onEntry(evt: Event, char: Any) = (evt, char) match {
        case (`digit`, ch: Char) ⇒ onDigit(ch)
        case (`dash`, _) ⇒ // Ignore
      }
      def onEntryX(evt: Event, char: Any) = (evt, char) match {
        case (`digit`, ch: Char) ⇒ onDigit(ch)
        case (`dash`, _) ⇒ // Ignore
    }
    def onDigit(char: Char) = num = (num * 10) + (char - '0')
  }
  val ParseYear = new ParseState
  val ParseMonth = new ParseState
  val ParseDay = new ParseState
  val Completed = new FinalState {
    var date: Option[java.sql.Date] = None
    override def onEntry(evt: Event) = evt match {
      case `done` ⇒ date = Some(new java.sql.Date(ParseYear.num - 1900, ParseMonth.num - 1, ParseDay.num))
    }
  }

  val transitions: Set[Transition] = Set(
    ParseYear -> digit -> ParseYear,
    ParseYear -> dash -> ParseMonth,
    ParseMonth -> digit -> ParseMonth,
    ParseMonth -> dash -> ParseDay,
    ParseDay -> digit -> ParseDay,
    ParseDay -> done -> Completed
  )
}

      }
