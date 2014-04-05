package scuff

import scala.annotation._
import java.lang.reflect.{ Array ⇒ _, _ }

/**
 * Helper class to operate on the internals
 * of an arbitrary object.
 * Useful for, among other things, setting
 * final fields on deserialization.
 * @author Nils Kilden-Pedersen
 */
class Surgeon[T <: AnyRef](patient: T) {

  private[this] val fields = Surgeon.getFields(patient.getClass)

  /**
   * Set field value reflectively.
   * @param name Field name
   * @param value Field value
   */
  def set(field: Symbol, value: Any): this.type = {
    fields(field).set(patient, value)
    this
  }

  /**
   * Get field value reflectively.
   * @param name Field name
   * @return Field value
   */
  def get[T](field: Symbol): T = fields(field).get(patient).asInstanceOf[T]

  def get[T](wantType: Class[T], exactClass: Boolean = false): Map[Symbol, T] = {
    val filtered = fields.filter {
      case (name, field) ⇒
        if (exactClass) {
          field.getType == wantType
        } else {
          //          val fieldType = if (field.getType.isPrimitive) scuff.primitiveToWrapper(field.getType) else field.getType
          wantType.isAssignableFrom(field.getType) ||
            field.getType.isPrimitive && wantType.isAssignableFrom(primitiveToWrapper(field.getType)) ||
            wantType.isPrimitive && primitiveToWrapper(wantType).isAssignableFrom(field.getType)
        }
    }
    filtered.map {
      case (name, field) ⇒
        name -> field.get(patient).asInstanceOf[T]
    }
  }
}

private object Surgeon {

  def getFields(cls: Class[_]): Map[Symbol, Field] = {
    fields.get(cls) match {
      case Some(map) ⇒ map
      case _ ⇒
        val map = new LockFreeConcurrentMap[Symbol, Field]
        saveFields(cls, map)
        fields.putIfAbsent(cls, map.snapshot) match {
          case None ⇒ map.snapshot
          case Some(other) ⇒ other
        }
    }
  }

  @annotation.tailrec
  private def saveFields(cls: Class[_], map: LockFreeConcurrentMap[Symbol, Field]) {
    if (cls != null) {
      cls.getDeclaredFields.foreach { field ⇒
        field.setAccessible(true)
        map.putIfAbsent(Symbol(field.getName), field)
      }
      saveFields(cls.getSuperclass, map)
    }
  }

  private val fields = new LockFreeConcurrentMap[Class[_], Map[Symbol, Field]]
}
