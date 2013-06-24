package scuff.web.form

import reflect.ClassTag
import scuff.Proxylicious
import util.Try
import java.lang.reflect.Method

object Parser {
  private def passthrough(s: String) = s
  private def toShort(s: String) = s.toShort
  private def toInt(s: String) = s.toInt
  private def toFloat(s: String) = s.toFloat
  private def toDouble(s: String) = s.toDouble
  private def toLong(s: String) = s.toLong
  private def toBD(s: String) = BigDecimal(s)
  private def toEmail(s: String) = new scuff.EmailAddress(s)
  private def toURL(s: String) = new java.net.URL(s)
  private def toBoolean(s: String) = s.toLowerCase match {
    case "true" | "1" | "yes" ⇒ true
    case "false" | "0" | "no" ⇒ false
  }
  private def toGeoPoint(s: String) = scuff.GeoPoint.parse(s).get

  val DefaultConverters: Map[Class[_], String ⇒ Any] = Map(
    classOf[String] -> passthrough,
    classOf[java.lang.Object] -> passthrough,
    classOf[Short] -> toShort,
    classOf[java.lang.Short] -> toShort,
    classOf[Int] -> toInt,
    classOf[java.lang.Integer] -> toInt,
    classOf[Float] -> toFloat,
    classOf[java.lang.Float] -> toFloat,
    classOf[Double] -> toDouble,
    classOf[java.lang.Double] -> toDouble,
    classOf[Long] -> toLong,
    classOf[java.lang.Long] -> toLong,
    classOf[BigDecimal] -> toBD,
    classOf[scuff.EmailAddress] -> toEmail,
    classOf[java.net.URL] -> toURL,
    classOf[Boolean] -> toBoolean,
    classOf[scuff.GeoPoint] -> toGeoPoint)
}

class Parser[T](implicit tag: ClassTag[T]) {

  require(tag.runtimeClass.isInterface, "Not an interface")

  private class ReturnType(cls: Class[_], genType: java.lang.reflect.Type) {
    private def genericType: Option[Class[_]] = genType match {
      case pm: java.lang.reflect.ParameterizedType ⇒ pm.getActualTypeArguments() match {
        case Array(cls: Class[_]) ⇒ Some(cls)
        case _ ⇒ None
      }
      case _ ⇒ None
    }
    def isOption = cls == classOf[Option[_]]
    def isList = cls.isAssignableFrom(classOf[List[_]])
    def convert(name: String, value: String): Any = {
      val convType = genericType.getOrElse(cls)
      Parser.this.convert(name, value, convType)
    }
  }

  private val getters = {
    val allMethods = tag.runtimeClass.getMethods()
    val getterMethods = allMethods.filter(_.getParameterTypes().length == 0)
    require(allMethods.length == getterMethods.length, "Only arg-less methods supported")
    getterMethods.map(m ⇒ m.getName -> new ReturnType(m.getReturnType, m.getGenericReturnType)).toMap
  }

  /**
   * This method will never be called with an empty/null string value,
   * so it's expected to always return a value or throw exception if unable
   * to do so.
   */
  protected def convert(name: String, value: String, toType: Class[_]): Any = {
    Parser.DefaultConverters.get(toType) match {
      case None ⇒ sys.error(s"""Cannot convert "$value" to $toType""")
      case Some(conv) ⇒ conv(value)
    }
  }

  def twoPass[A](form: Map[String, Seq[String]])(secondPass: T ⇒ Either[Set[Problem], A]): Either[Set[Problem], A] = {
    var values: Map[String, Any] = Map.empty
    var errors: Set[Problem] = Set.empty
    getters.foreach {
      case (name, rt) ⇒
        def convertOrFail(conv: ⇒ Any) {
            try {
              values += name -> conv
            } catch {
              case e: Exception ⇒ errors += Invalid(name, e)
            }
          }
        val withContent = {
          val list = form.get(name).getOrElse(Nil).toList
          list.flatMap {
            _.trim match {
              case "" ⇒ None
              case s ⇒ Some(s)
            }
          }
        }
        withContent match {
          case Nil ⇒
            if (rt.isList) {
              values += (name -> Nil)
            } else if (rt.isOption) {
              values += (name -> None)
            } else {
              errors += Required(name)
            }
          case head :: _ ⇒
            if (rt.isOption) {
              convertOrFail(Option(rt.convert(name, head)))
            } else if (rt.isList) {
              convertOrFail(withContent.map(rt.convert(name, _)))
            } else {
              convertOrFail(rt.convert(name, head))
            }
        }
    }
    if (errors.isEmpty) {
        def handler(t: T, method: Method, args: Array[AnyRef]): Any = {
          assert(args == null || args.length == 0)
          values(method.getName)
        }
      val p = new scuff.Proxylicious[T]
      val t = p.proxify(handler)
      secondPass(t)
    } else Left(errors)
  }
  def onePass(form: Map[String, Seq[String]]): Either[Set[Problem], T] = twoPass[T](form)(t ⇒ Right(t))

}
