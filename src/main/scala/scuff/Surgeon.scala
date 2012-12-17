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

  private val patientType: Class[T] = patient.getClass.asInstanceOf[Class[T]]

  private def findField(name: Symbol): Field = {

      @tailrec
      def find(name: String, fromClass: Class[_ >: T]): Field = {

          @tailrec
          def check(idx: Int, fields: Array[Field]): Field = {
            if (idx == fields.length) {
              null
            } else if (fields(idx).getName == name) {
              fields(idx)
            } else {
              check(idx + 1, fields)
            }
          }

        if (fromClass == null) {
          null
        } else {
          check(0, fromClass.getDeclaredFields) match {
            case null ⇒ find(name, fromClass.getSuperclass)
            case field ⇒ field.setAccessible(true); field
          }
        }

      }
    find(name.name, patientType)
  }

  /**
   * Set field value reflectively.
   * @param name Field name
   * @param value Field value
   */
  def setField(name: Symbol, value: Any) {
    findField(name).set(patient, value)
  }
  /**
   * Get field value reflectively.
   * @param name Field name
   * @return Field value
   */
  def getField[T](name: Symbol): T = findField(name).get(patient).asInstanceOf[T]
}

object Surgeon {
  
  private def isSerializable(f: Field) = !Modifier.isStatic(f.getModifiers) && !Modifier.isTransient(f.getModifiers)
  
  /**
   * Check if class has non-transient, non-static fields declared.
   */
  def hasSerializableFields(cls: Class[_]): Boolean = {
    if (cls == null) {
      false
    } else {
      val fields = cls.getDeclaredFields
      if (fields.length == 0) {
        hasSerializableFields(cls.getSuperclass)
      } else if (isSerializable(fields(0))) {
        true
      } else if (fields.filterNot(isSerializable).length == fields.length) { // All fields are not serializable
        hasSerializableFields(cls.getSuperclass)
      } else {
        true
      }
    }
  }
}