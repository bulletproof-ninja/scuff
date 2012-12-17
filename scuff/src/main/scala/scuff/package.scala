import java.util.Locale

package object scuff {

  private val localeSplitter = "[_-]".r

  implicit def str2Locale(str: String) = localeSplitter.split(str) match {
    case Array() => Locale.ROOT
    case Array(lang) ⇒ new Locale(lang)
    case Array(lang, ctry) ⇒ new Locale(lang, ctry)
    case Array(lang, ctry, variant) ⇒ new Locale(lang, ctry, variant)
  }
}