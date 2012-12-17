package scuff

import org.junit._
import org.junit.Assert._
import java.util.Locale
import java.nio.charset.Charset

class SomeText(locale: Locale) extends PropertiesFormatter(locale)

class TestPropertiesFormatter {
  import PropNameEnum._

  @Test
  def root {
    val props = PropertiesFormatter.root("FooBar")
    assertEquals("Hello", props("say"))
  }
  
  @Test
  def german() {
    val german = new SomeText(Locale.GERMAN)
    val text = german(BeerRequest.name, "Zwei")
    assertEquals("Zwei Bier, bitte", text)
  }

  @Test
  def english() {
    val english = new SomeText(Locale.US)
    val text = english(BeerRequest.name, "Two")
    assertEquals("Two beers, please. I said Two beers please.", text)
  }

  @Test
  def danish() {
    val danish = new SomeText(new Locale("da"))
    val text = danish(BeerRequest.name, "To")
    assertEquals("To øl, tak", text)
  }

  @Test
  def spanish() {
    val spanish = new SomeText(new Locale("es"))
    val text = spanish(BeerRequest.name, "Dos")
    assertEquals("Dos cerveza, por favor", text)
  }

  @Test
  def multiple() {
    val anglais = new SomeText(Locale.ENGLISH)
    val text = anglais("two.parms", "One", "Two")
    assertEquals("And I call them. Thing One and Thing Two", text)
  }

  @Test
  def missing() {
    val portuguese = new SomeText(new Locale("pt"))
    assertEquals("42,00", portuguese("number", 42))
  }

  @Test
  def australian() {
    val oz = new SomeText(new Locale("en", "AU"))
    val t1t2 = oz("two.parms", "One", "Two")
    assertEquals("And I call them. Thing One and Thing Two", t1t2)
    val beer = oz(BeerRequest.name, "Two")
    assertEquals("Two beers, mate. Oy, I said Two beers matey.", beer)
  }
  
  @Test
  def number() {
    val de = new SomeText(Locale.GERMAN)
    val en = new SomeText(Locale.ENGLISH)
    val number = 5432.1f
    assertEquals("5.432,10", de("number", number))
    assertEquals("5,432.10", en("number", number))
  }
  
  @Test
  def apostrophes {
    val en = new SomeText(Locale.ENGLISH)
    val greet1 = en("welcome")
    assertEquals("You're welcome!", greet1)
    val greet2 = en("welcome_name", "Nils")
    assertEquals("You're welcome, Nils!", greet2)
  }
}

sealed abstract class PropNameEnum(val name: String)

object PropNameEnum {
  case object BeerRequest extends PropNameEnum("text.beer_request")
}