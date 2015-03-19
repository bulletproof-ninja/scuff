package scuff

import org.junit._
import org.junit.Assert._
import java.util.Locale
import java.nio.charset.Charset

class TestL10nPropFormatter {

  def SomeText(locale: Locale*) = L10nPropFormatter(Some(getClass.getPackage), "SomeText", locale, L10nPropFormatter.ISO_8859_1)

  sealed abstract class PropNameEnum(val name: String)

  object PropNameEnum {
    case object BeerRequest extends PropNameEnum("text.beer_request")
  }

  import PropNameEnum._

  @Test
  def `timestamp` {
    val foo = SomeText()
    val now = new Timestamp
    val text = foo("iso", now)
    assertEquals(s"Date: $now", text)
  }

  @Test
  def root {
    val props = L10nPropFormatter.root("FooBar")
    assertEquals("Hello", props("say"))
  }

  @Test
  def german() {
    val german = SomeText(Locale.GERMAN, Locale.CANADA_FRENCH)
    val text = german(BeerRequest.name, "Zwei")
    assertEquals("Zwei Bier, bitte", text)
  }

  @Test
  def english() {
    val english = SomeText(Locale.US)
    val text = english(BeerRequest.name, "Two")
    assertEquals("Two beers, please. I said Two beers please.", text)
  }

  @Test
  def danish() {
    val danish = SomeText(new Locale("da"))
    val text = danish(BeerRequest.name, "To")
    assertEquals("To øl, tak", text)
  }

  @Test
  def spanish() {
    val spanish = SomeText(new Locale("es"))
    val text = spanish(BeerRequest.name, "Dos")
    assertEquals("Dos cerveza, por favor", text)
  }

  @Test
  def multiple() {
    val anglais = SomeText(Locale.ENGLISH)
    val text = anglais("two.parms", "One", "Two")
    assertEquals("And I call them. Thing One and Thing Two", text)
  }

  @Test
  def missing() {
    val portuguese = SomeText(new Locale("pt"))
    assertEquals("42,00", portuguese("number", 42))
  }

  @Test
  def australian() {
    val oz = SomeText(new Locale("en", "AU"))
    val t1t2 = oz("two.parms", "One", "Two")
    assertEquals("And I call them. Thing One and Thing Two", t1t2)
    val beer = oz(BeerRequest.name, "Two")
    assertEquals("Two beers, mate. Oy, I said Two beers matey.", beer)
  }

  @Test
  def number() {
    val de = SomeText(Locale.GERMAN)
    val en = SomeText(Locale.ENGLISH)
    val number = 5432.1f
    assertEquals("5.432,10", de("number", number))
    assertEquals("5,432.10", en("number", number))
  }

  @Test
  def apostrophes {
    val en = SomeText(Locale.ENGLISH)
    val greet1 = en("welcome")
    assertEquals("You're welcome!", greet1)
    val greet2 = en("welcome_name", "Nils")
    assertEquals("You're welcome, Nils!", greet2)
  }

  @Test
  def escaping {
    val foo = SomeText()
    val text = foo("escape_confusion", "John Mapplethorpe", "a jewel thief", "that one movie")
    assertEquals("You're John Mapplethorpe, right? Didn't you play a jewel thief in that one movie?", text)
  }

  @Test
  def `local lang` {
    val en = SomeText()
    val eng = en("do_you_speak", Locale.FRENCH)
    assertEquals("Do you speak French?", eng)
    val de = SomeText(Locale.forLanguageTag("de"))
    val ger = de("do_you_speak", Locale.FRENCH)
    assertEquals("Sprechen Sie Französisch?", ger)
  }

  @Test
  def `function parm` {
      def twoLanguages(lang1: Locale, lang2: Locale)(fmtLang: Locale) = SomeText(fmtLang)("two_languages", lang1, lang2)
    val foo = SomeText()
    val text = foo("do_you_speak", twoLanguages(Locale.ENGLISH, Locale.FRENCH) _)
    assertEquals("Do you speak both English AND French?", text)
  }

  @Test
  def `nested` {
    val foo = SomeText()
    val text = foo("do_you_speak", foo("two_languages", Locale.ENGLISH, java.util.Locale.FRENCH))
    assertEquals("Do you speak both English AND French?", text)
  }

}
