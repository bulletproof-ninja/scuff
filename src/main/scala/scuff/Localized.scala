package scuff

import java.util.Locale

object Localized {

  final class Key private[Localized] (private[Localized] val locales: Seq[Locale])

  def makeKey(locales: Locale*) = new Key(locales ++ stripCountries(locales))

  private def stripCountries(locales: Seq[Locale]): Seq[Locale] = locales.flatMap { l ⇒
    if (l.getCountry == "") {
      None
    } else {
      Some(new Locale(l.getLanguage))
    }
  }
}

/**
 * Container for localized content,
 * with default fallback.
 */
class Localized[T](val byLocale: Map[Locale, T], val defaultLocale: Locale) extends Serializable {
  require(byLocale.contains(defaultLocale), "Must contain default locale %s: %s".format(defaultLocale, byLocale.keySet))
  def this(byLocale: Map[Locale, T]) = this(byLocale, byLocale.head._1)
  def this(default: (Locale, T), others: (Locale, T)*) = this((others :+ default).toMap, default._1)

  def makeKey(locales: Locale*) = Localized.makeKey(locales: _*)
  def apply(locales: Locale*): T = apply(makeKey(locales: _*))
  def apply(key: Localized.Key): T = findT(key) match {
    case None ⇒ byLocale(defaultLocale)
    case Some(t) ⇒ t
  }

  private def findT(key: Localized.Key): Option[T] = {
    val i = key.locales.iterator
    var value: Option[T] = None
    while (value.isEmpty && i.hasNext) {
      value = byLocale.get(i.next)
    }
    value
  }

  override def toString = "%s: %s".format(defaultLocale.toLanguageTag, byLocale(defaultLocale))

}
