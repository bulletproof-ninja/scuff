package scuff.reflect

import java.lang.reflect.Field

import scala.reflect.{ ClassTag, classTag }

import language.dynamics

/**
  * Helper class to operate on the internals
  * of an arbitrary object.
  * Useful for, among other things, setting
  * final fields on deserialization.
  */
class Surgeon[T <: AnyRef](patient: T)
  extends Dynamic {

  private[this] val fields = Surgeon.fields.get(patient.getClass)

  /**
    * Set field value reflectively.
    * @param name Field name
    * @param value Field value
    */
  def updateDynamic(name: String)(value: Any) = {
    fields(name).set(patient, value)
  }

  /**
    * Get field value reflectively.
    * @param name Field name
    * @return Field value
    */
  def selectDynamic(field: String): Any = fields(field).get(patient)

  def hasField(field: String): Boolean = fields.contains(field)

  def getAll[F: ClassTag]: Map[String, F] = getAll[F](false)
  def getAll[F: ClassTag](exactClass: Boolean): Map[String, F] = {
    val wantType = classTag[F].runtimeClass
    val filtered = fields.filter {
      case (_, field) =>
        if (exactClass) {
          field.getType == wantType
        } else {
          wantType.isAssignableFrom(field.getType) ||
            field.getType.isPrimitive && wantType.isAssignableFrom(primitiveToWrapper(field.getType)) ||
            wantType.isPrimitive && primitiveToWrapper(wantType).isAssignableFrom(field.getType)
        }
    }
    filtered.map {
      case (name, field) =>
        name -> field.get(patient).asInstanceOf[F]
    }
  }
}

private object Surgeon {

  private def extractFields(cls: Class[_], map: Map[String, Field] = Map.empty): Map[String, Field] = {
    if (cls == null) {
      map
    } else {
      cls.getDeclaredFields.foldLeft(extractFields(cls.getSuperclass, map)) {
        case (map, field) =>
          field.setAccessible(true)
          map.updated(field.getName, field)
      }
    }
  }

  val fields = new ClassValue[Map[String, Field]] {
    def computeValue(cls: Class[_]) = extractFields(cls)
  }
}
