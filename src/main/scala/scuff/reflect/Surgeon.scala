package scuff.reflect

import scala.annotation._
import java.lang.reflect._
import scuff.concurrent.LockFreeConcurrentMap

/**
  * Helper class to operate on the internals
  * of an arbitrary object.
  * Useful for, among other things, setting
  * final fields on deserialization.
  * @author Nils Kilden-Pedersen
  */
class Surgeon[T <: AnyRef](patient: T) {

  private[this] val fields = Surgeon.fields.get(patient.getClass)

  /**
    * Set field value reflectively.
    * @param name Field name
    * @param value Field value
    */
  def set(field: Symbol, value: Any): this.type = {
    fields(field).set(patient, value)
    this
  }

  def has(field: Symbol): Boolean = fields.contains(field)

  /**
    * Get field value reflectively.
    * @param name Field name
    * @return Field value
    */
  def get[T](field: Symbol): T = fields(field).get(patient).asInstanceOf[T]

  def get[T](wantType: Class[T], exactClass: Boolean = false): Map[Symbol, T] = {
    val filtered = fields.filter {
      case (name, field) =>
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
        name -> field.get(patient).asInstanceOf[T]
    }
  }
}

private object Surgeon {

  private def extractFields(cls: Class[_], map: Map[Symbol, Field] = Map.empty): Map[Symbol, Field] = {
    if (cls == null) {
      map
    } else {
      cls.getDeclaredFields.foldLeft(extractFields(cls.getSuperclass, map)) {
        case (map, field) =>
          field.setAccessible(true)
          map.updated(Symbol(field.getName), field)
      }
    }
  }

  val fields = new ClassValue[Map[Symbol, Field]] {
    def computeValue(cls: Class[_]) = extractFields(cls)
  }
}
