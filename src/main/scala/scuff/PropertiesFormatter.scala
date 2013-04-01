package scuff

import java.util.{ Locale, ResourceBundle, PropertyResourceBundle, MissingResourceException, MissingFormatArgumentException }
import java.text.MessageFormat
import java.nio.charset.Charset

/**
 * Class that combines a `.properties` file with a formatter.
 * <p>Example:
 * {{{
 *   class Text extends PropertiesFormatter
 * }}}
 * Create file `Text.properties` in the same package as class `Text`:
 * {{{
 *   msg.hello = Hello World! Today is {0}
 * }}}
 * And, voila, the property values are accessible through `apply`:
 * {{{
 *   val messages = new Text
 *   val msg = messages("label.hello", new java.util.Date)
 *   println(msg)
 * }}}
 * This class will have fallback behavior per key.
 * This means that more specific locale files (country and variant)
 * need not have all the keys, but merely the keys that differ.
 * @see java.text.MessageFormat
 * For text formatting syntax
 * @see java.util.PropertyResourceBundle
 * For locale specific `.properties` file name syntax
 * @author Nils Kilden-Pedersen
 */
class PropertiesFormatter private (_baseName: Option[String], desiredLocale: Locale, charset: Charset) {

  /**
    * Subclass constructor. Used when sub-classing is used for location and naming.
    * @param desiredLocale The desired locale. May use a fallback locale if specific locale is not found
    * @param charset Override default properties charset of UTF-8
    */
  protected def this(desiredLocale: Locale = Locale.ROOT, charset: Charset = PropertiesFormatter.ISO_8859_1) = this(None, desiredLocale, charset)

  private[this] val control = new CharsetControl(charset)

  private[this] val baseName = _baseName match {
    case Some(bn) ⇒ bn
    case None ⇒ {
      val name = getClass.getName
      if (name endsWith "$") name.substring(0, name.length - 1) else name
    }
  }

  private class Message(strFmt: String) {
    val parmCount = PropertiesFormatter.countParms(strFmt)
    val stringFormat = if (parmCount == 0) strFmt.replace("''", "'") else strFmt
    val formatter = new ThreadLocal[MessageFormat]() {
      override def initialValue = new MessageFormat(stringFormat, desiredLocale)
    }
    override def toString = strFmt
  }

  private val map: Map[String, Message] = {
      def findText(key: String, bundles: Seq[ResourceBundle]): Option[String] = {
        var text: String = null
        val iter = bundles.iterator
        while (text == null && iter.hasNext) try {
          text = iter.next.getString(key)
        } catch {
          case _: MissingResourceException ⇒ // Ignore
        }
        Option(text)
      }
    val bundles = PropertiesFormatter.toResourceBundles(baseName, desiredLocale, control)
    var map: Map[String, Message] = Map.empty
    val iter = bundles.flatMap(e ⇒ collection.JavaConversions.enumerationAsScalaIterator(e.getKeys).toSeq).toSet.iterator
    while (iter.hasNext) {
      val key = iter.next
      findText(key, bundles).foreach { text ⇒
        map += (key -> new Message(text))
      }
    }
    map
  }

  /**
   * Lookup string based on the supplied key, and optionally
   * perform formatting if parameters are supplied.
   * @param key The lookup key
   * @param parms The optional formatting parameters
   * @return The string, formatted if applicable
   * @throws MissingResourceException if key is unknown
   * @throws MissingFormatArgumentException
   * if the number of supplied parameters do not match the expected number of parameters
   * @throws IllegalArgumentException if the parameters otherwise fail to format the message
   */
  def apply(key: String, parms: Any*): String = this.get(key, parms: _*) match {
    case None ⇒ throw new MissingResourceException("Cannot find \"" + key + "\"", baseName, key)
    case Some(msg) ⇒ msg
  }

  def get(key: String, parms: Any*): Option[String] = map.get(key).map { msg ⇒
    if (msg.parmCount != parms.length) {
      throw new MissingFormatArgumentException(
        "Message '%s' expects %d parameters, but received %d".format(
          key, msg.parmCount, parms.length))
    }
    if (msg.parmCount == 0) {
      msg.stringFormat
    } else {
      msg.formatter.get.format(parms.toArray)
    }
  }

  def keySet() = map.keySet

  lazy val unformatted: Map[String, String] = map.map {
    case (key, msg) ⇒ key -> msg.toString
  }

}

object PropertiesFormatter {

  final val ISO_8859_1 = Charset.forName("ISO-8859-1")
  final val UTF_8 = Charset.forName("UTF-8")

  private val parmFinder = java.util.regex.Pattern compile """\{(\d+)[^\}]*\}"""
  private def countParms(format: CharSequence): Int = {
    var set = collection.SortedSet()(scala.math.Ordering.Int.reverse)
    val matcher = parmFinder.matcher(format)
    while (matcher.find) {
      set += matcher.group(1).toInt
    }
    set.headOption.foreach { h ⇒
      if (h + 1 != set.size) throw new IndexOutOfBoundsException("Parameter mismatch: \"" + format + "\"")
    }
    set.size
  }
  private def toResourceBundles(baseName: String, locale: Locale, control: CharsetControl): Seq[ResourceBundle] = {
      def appendBundle(buffer: collection.mutable.Buffer[ResourceBundle], locale: Locale) {
        try {
          val bundle = ResourceBundle.getBundle(baseName, locale, control)
          if (bundle.getLocale == locale) buffer += bundle
        } catch {
          case e: MissingResourceException ⇒ // Ignore
        }
      }
    val buffer = new collection.mutable.ArrayBuffer[ResourceBundle](4)
    appendBundle(buffer, locale)
    if (locale.getVariant.length > 0) {
      appendBundle(buffer, new Locale(locale.getLanguage, locale.getCountry))
    }
    if (locale.getCountry.length > 0) {
      appendBundle(buffer, new Locale(locale.getLanguage))
    }
    if (locale.getLanguage.length > 0) {
      appendBundle(buffer, Locale.ROOT)
    }
    if (buffer.isEmpty) {
      throw new IllegalStateException("Cannot find resource for: " + baseName)
    }
    buffer
  }

  /**
   * Construct instance from specific package, using non-class based name.
   * @param location The package location
   * @param baseName The `.properties` base name.
   * @param desiredLocale The preferred locale
   * @param charset The properties encoding
   * @return new instance
   */
  def apply(location: Option[Package], baseName: String, desiredLocale: Locale, charset: Charset) = {
    val packagePrefix = location.map(_.getName concat ".").getOrElse("")
    new PropertiesFormatter(Some(packagePrefix concat baseName), desiredLocale, charset)
  }
  
  def root(name: String, desiredLocale: Locale = Locale.getDefault, charset: Charset = ISO_8859_1) = {
    new PropertiesFormatter(Some(name), desiredLocale, charset)
  }

  /**
   * Construct instance to match the provided class, which is not a
   * subclass of [[PropertiesFormatter]].
   * @param baseName The class which name will match `.properties` file in same package
   * @param desiredLocale The preferred locale
   * @return new instance
   */
  def apply(baseName: Class[_], desiredLocale: Locale = Locale.getDefault, charset: Charset = ISO_8859_1) =
    new PropertiesFormatter(Some(baseName.getName), desiredLocale, charset)

}

private class CharsetControl(charset: Charset) extends ResourceBundle.Control {
  import java.io._
  import java.security._
  override def newBundle(baseName: String, locale: Locale, format: String, loader: ClassLoader, reload: Boolean): ResourceBundle = {
    val bundleName = toBundleName(baseName, locale)
    var bundle: ResourceBundle = null
    if (format.equals("java.class")) {
      try {
        val bundleClass = loader.loadClass(bundleName).asInstanceOf[Class[_ <: ResourceBundle]]
        // If the class isn't a ResourceBundle subclass, throw a
        // ClassCastException.
        if (classOf[ResourceBundle].isAssignableFrom(bundleClass)) {
          bundle = bundleClass.newInstance()
        } else {
          throw new ClassCastException(bundleClass.getName()
            + " cannot be cast to ResourceBundle")
        }
      } catch {
        case _: ClassNotFoundException ⇒ // Ignore
      }
    } else if (format.equals("java.properties")) {
      val resourceName = toResourceName(bundleName, "properties")
      val classLoader = loader
      val reloadFlag = reload
      var stream: InputStream = null
      try {
        stream = AccessController.doPrivileged(
          new PrivilegedExceptionAction[InputStream] {
            def run: InputStream = {
              var is: InputStream = null
              if (reloadFlag) {
                val url = classLoader.getResource(resourceName)
                if (url != null) {
                  val connection = url.openConnection()
                  if (connection != null) {
                    // Disable caches to get fresh data for
                    // reloading.
                    connection.setUseCaches(false)
                    is = connection.getInputStream()
                  }
                }
              } else {
                is = classLoader.getResourceAsStream(resourceName)
              }
              return is
            }
          })
      } catch {
        case e: PrivilegedActionException ⇒ throw e.getException()
      }
      if (stream != null) {
        try {
          bundle = new PropertyResourceBundle(new InputStreamReader(stream, charset))
        } finally {
          stream.close()
        }
      }
    } else {
      throw new IllegalArgumentException("unknown format: " + format)
    }
    return bundle
  }
}