package scuff

import scala.reflect.ClassTag

trait EnumValue {
  value: Enum[EnumValue]#Value =>

  def id: Int
}

/**
 * Parametric `scala.Enumeration` extension.
 * @tparam E Sealed trait enum type
 */
class Enum[V <: EnumValue: ClassTag] extends Enumeration {

  type Value = V

  lazy val list: List[V] = this.values.toList.collect {
    case v: V => v
  }

  def get(name: String): Option[V] = list.find(_.toString == name)

  def apply(name: String): V = {
    get(name) match {
      case Some(value) => value
      case _ =>
        val valuesStr = this.list.map(v => s"'$v'").mkString(", ")
        throw new NoSuchElementException(s"No value found for '$name'; available: $valuesStr")
    }
  }

}
