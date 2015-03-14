package scuff

import java.util.Locale

object Localized {

  final class Key private[Localized] (private[Localized] val locales: Seq[Locale])

  def makeKey(locales: Locale*) = new Key(locales ++ stripCountries(locales))

  private def stripCountries(locales: Seq[Locale]): Seq[Locale] = locales.flatMap { l =>
    if (l.getCountry == "") {
      None
    } else {
      Some(new Locale(l.getLanguage))
    }
  }

  private def getArbitraryLanguage(map: Map[Locale, _]): Locale = {
    if (map.isEmpty) {
      throw new IllegalArgumentException("Language map is empty!")
    }
    map.head._1
  }
}

/**
 * Container for localized content,
 * with default fallback.
 */
class Localized[T](val byLang: Map[Locale, T], val defaultLang: Locale) extends Serializable {
  require(byLang.contains(defaultLang), "Must contain default locale %s: %s".format(defaultLang, byLang.keySet))
  def this(byLang: Map[Locale, T]) = this(byLang, Localized.getArbitraryLanguage(byLang))
  def this(default: (Locale, T), others: (Locale, T)*) = this((others :+ default).toMap, default._1)

  def makeKey(locales: Locale*) = Localized.makeKey(locales: _*)
  def apply(locales: Locale*): T = apply(makeKey(locales: _*))
  def apply(key: Localized.Key): T = findT(key) match {
    case None => byLang(defaultLang)
    case Some(t) => t
  }

  private def findT(key: Localized.Key): Option[T] = {
    val i = key.locales.iterator
    var value: Option[T] = None
    while (value.isEmpty && i.hasNext) {
      value = byLang.get(i.next)
    }
    value
  }

  override def toString = "%s: %s".format(defaultLang.toLanguageTag, byLang(defaultLang))

}
