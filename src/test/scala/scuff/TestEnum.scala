package scuff

import org.junit._, Assert._

class TestEnum {

  sealed trait WeekDay
  object WeekDay extends Enum[WeekDay] {
    val Mon, Tue, Wed, Thu, Fri, Sat, Sun = new Val with WeekDay

    val Foo, Bar = new Val

    def isWorkingDay(d: WeekDay) = d != Sat && d != Sun
  }

  @Test
  def `trait only`(): Unit = {
    assertEquals(9, WeekDay.values.size)
    assertEquals(7, WeekDay.list.size)
  }

  @Test
  def weekend(): Unit = {
    val weekend = WeekDay.list.filterNot(WeekDay.isWorkingDay)
    assertEquals(2, weekend.size)
    assertTrue(weekend contains WeekDay.Sat)
    assertTrue(weekend contains WeekDay.Sun)
  }

  @Test(expected = classOf[NoSuchElementException])
  def `valueOf not found`(): Unit = {
    assertEquals(WeekDay.Mon, WeekDay valueOf "Foo")
  }

  @Test
  def `valueOf found`(): Unit = {
    assertEquals(WeekDay.Mon, WeekDay valueOf "Mon")
    assertTrue(WeekDay isWorkingDay (WeekDay valueOf "Thu"))
    assertFalse(WeekDay isWorkingDay (WeekDay valueOf "Sun"))
  }

  @Test
  def bigPlanets(): Unit = {
    import Planet._
    val big = Planet.list.filter(_.radius > 7.0e6)
    assertEquals(Set(Jupiter, Saturn, Uranus, Neptune), big.toSet)
  }

  @Test
  def `match may not be exhaustive`(): Unit = {
    val planet: Planet = Planet.Earth
    planet match {
      case Planet.Earth => // Success
    }
  }

  sealed trait Planet {
    def mass: Double
    def radius: Double
    def surfaceGravity: Double = Planet.G * mass / (radius * radius)
    def surfaceWeight(otherMass: Double): Double = otherMass * surfaceGravity
  }
  object Planet extends Enum[Planet] {
    protected case class Val(mass: Double, radius: Double) extends super.Val with Planet

    val G: Double = 6.67300E-11

    val Mercury = Val(3.303e+23, 2.4397e6)
    val Venus = Val(4.869e+24, 6.0518e6)
    val Earth = Val(5.976e+24, 6.37814e6)
    val Mars = Val(6.421e+23, 3.3972e6)
    val Jupiter = Val(1.9e+27, 7.1492e7)
    val Saturn = Val(5.688e+26, 6.0268e7)
    val Uranus = Val(8.686e+25, 2.5559e7)
    val Neptune = Val(1.024e+26, 2.4746e7)
  }

}
