package scuff.web

import java.lang.reflect._
import java.beans._

/**
  * Form parser.
  */
abstract class FormParser[B](implicit manifest: ClassManifest[B]) {

  private val beanClass = manifest.erasure.asInstanceOf[Class[B]]
  protected def onMissing(prop: Symbol): String = "required"
  protected def onError(prop: Symbol, e: Exception): String = e.getMessage

  protected def setDerived(errorName: Symbol)(code: ⇒ Unit): Option[(Symbol, String)] = {
    require(setters.contains(errorName), "Unknown property name: " + errorName)
    try {
      code
      None
    } catch {
      case e: Exception ⇒ Some(errorName -> onError(errorName, e))
    }
  }

  private case class Property(actualType: Type, converterMethod: ((B, String) ⇒ Any), setterMethod: Method)

  private val setters: Map[Symbol, Property] = Introspector.getBeanInfo(beanClass, classOf[Object]).getPropertyDescriptors.collect {
    case pd if pd.getWriteMethod != null ⇒
      val convType: Class[_] = if (pd.getPropertyType == classOf[Option[_]] || pd.getPropertyType == classOf[Seq[_]]) {
        pd.getWriteMethod.getGenericParameterTypes.head match {
          case pt: ParameterizedType ⇒ pt.getActualTypeArguments.head.asInstanceOf[Class[_]]
          case _ ⇒ classOf[Any]
        }
      } else {
        pd.getPropertyType
      }
      val convMethod = if (convType == classOf[String]) {
        (bean: B, str: String) ⇒ str
      } else try {
        val converter = beanClass.getMethod(pd.getName, classOf[String])
        if (converter.getReturnType.isPrimitive || convType.isAssignableFrom(converter.getReturnType)) {
          (bean: B, str: String) ⇒ converter.invoke(bean, str)
        } else {
          throw new NoSuchMethodException
        }
      } catch {
        case nsm: NoSuchMethodException ⇒
          val returnName = if (convType == classOf[Any]) "T" else if (convType.isPrimitive) convType.getName.substring(0, 1).toUpperCase + convType.getName.substring(1) else convType.getName
          throw new IllegalStateException("Cannot find expected converter method: def %s(String): %s".format(pd.getName, returnName))
      }
      Symbol(pd.getName) -> Property(pd.getPropertyType, convMethod, pd.getWriteMethod)
  }.toMap

  def parseStrKeys(form: String ⇒ Seq[String]): Either[Map[String, String], B] = {
    val withSymbol = (prop: Symbol) ⇒ form(prop.name)
    parse(withSymbol) match {
      case Left(errors) ⇒ Left(errors.map(entry ⇒ entry._1.name -> entry._2))
      case Right(r) ⇒ Right(r)
    }
  }

  /**
    * Parse form and produce either a correctly populated bean object,
    * or a map of error messages per property.
    */
  def parse(form: Symbol ⇒ Seq[String]): Either[Map[Symbol, String], B] = {
    val bean = beanClass.newInstance
    val errors = firstPass(bean, form)
    if (errors.isEmpty) {
      val errors = secondPass(bean)
      if (errors.isEmpty)
        Right(bean)
      else
        Left(errors)
    } else
      Left(errors)
  }

  private def set(bean: B, name: Symbol, method: Method, arg: Any): Option[String] = try {
    try {
      method.invoke(bean, arg.asInstanceOf[AnyRef])
    } catch {
      case e: InvocationTargetException ⇒ throw e.getTargetException
    }
    None
  } catch {
    case e: Exception ⇒ Some(onError(name, e))
  }

  protected def firstPass(bean: B, form: Symbol ⇒ Seq[String]): Map[Symbol, String] = {
    var errors = Map[Symbol, String]()
    setters.foreach {
      case (name, Property(propType, converter, setter)) ⇒
        val strValues = {
          val seq = try { form(name) } catch { case _ ⇒ Seq.empty }
          seq.flatMap {
            _.trim match {
              case "" ⇒ None
              case s ⇒ Some(s)
            }
          }
        }
        val setValue: Either[String, Any] = try {
          if (propType == classOf[Seq[_]]) {
            Right(strValues.map(str ⇒ converter(bean, str)))
          } else if (propType == classOf[Option[_]]) {
            Right(strValues.headOption.map(str ⇒ converter(bean, str)))
          } else {
            strValues.headOption match {
              case None ⇒ Left(onMissing(name))
              case Some(str) ⇒ Right(converter(bean, str))
            }
          }
        } catch {
          case e: Exception ⇒ Left(onError(name, e))
        }
        setValue match {
          case Left(error) ⇒ errors += name -> error
          case Right(value) ⇒ set(bean, name, setter, value) match {
            case Some(error) ⇒ errors += name -> error
            case None ⇒ // Succeeded
          }
        }
    }
    errors
  }

  protected val secondPasses: Map[Symbol, (B) ⇒ Option[String]] = Map.empty

  protected def secondPass(bean: B): Map[Symbol, String] = {
    var errors = Map[Symbol, String]()
    secondPasses.foreach {
      case (prop, sp) ⇒ try {
        sp(bean).foreach { error ⇒
          errors += prop -> error
        }
      } catch {
        case e: Exception ⇒ errors += prop -> onError(prop, e)
      }
    }
    errors
  }

}
