package scuff

import scala.reflect.ClassTag

/**
 * Parametric `scala.Enumeration` extension.
 * @tparam E Sealed trait enum type
 */
class Enum[E: ClassTag] extends Enumeration {
  type Value = super.Value with E

  lazy val list: List[Value] = this.values.toList.filter {
    case _: E => true
    case _ => false
  }.asInstanceOf[List[Value]]

  def valueOf(name: String): Value = {
    list.find(_.toString == name) match {
      case Some(value) => value
      case _ =>
        val valuesStr = this.list.map(v => s"'$v'").mkString(", ")
        throw new NoSuchElementException(s"No value found for '$name'; available: $valuesStr")
    }
  }
}
