package scuff

import java.util.Locale

class Localized[T](val byLocale: Map[Locale, T], val defaultLocale: Locale) extends Serializable {
  def this(byLocale: Map[Locale, T]) = this(byLocale, byLocale.head._1)
  def this(default: (Locale, T), others: (Locale, T)*) = this((others :+ default).toMap, default._1)

  def apply(locales: Locale*): T = findT(locales ++ stripCountries(locales)) match {
    case None ⇒ byLocale(defaultLocale)
    case Some(t) ⇒ t
  }

  private def stripCountries(locales: Seq[Locale]): Seq[Locale] = locales.flatMap { l ⇒
    if (l.getCountry == "") {
      None
    } else {
      Some(new Locale(l.getLanguage))
    }
  }

  private def findT(locales: Seq[Locale]): Option[T] = {
    val i = locales.iterator
    var value: Option[T] = None
    while (value.isEmpty && i.hasNext) {
      value = byLocale.get(i.next)
    }
    value
  }

  override def toString = "%s: %s".format(defaultLocale.toLanguageTag, byLocale(defaultLocale))

}