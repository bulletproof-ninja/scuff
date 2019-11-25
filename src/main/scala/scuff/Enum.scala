package scuff

import scala.reflect.ClassTag

object Enum {
  trait Value {
    value: Enum[Enum.Value]#Value =>

    def id: Int
    final def name: String = this.toString

  }
}

/**
 * Parametric `scala.Enumeration` extension.
 * @tparam E Sealed trait enum type
 */
class Enum[V <: Enum.Value: ClassTag] extends Enumeration {

  type Value = V

  lazy val list: List[V] = this.values.toList.collect {
    case v: V => v
  }

  def find(find: V => Boolean): Option[V] = list.find(find)

  def get(name: String): Option[V] = list.find(_.name == name)

  def apply(name: String): V = {
    get(name) match {
      case Some(value) => value
      case _ =>
        val valuesStr = this.list.map(v => s"'$v'").mkString(", ")
        throw new NoSuchElementException(s"No value found for '$name'; available: $valuesStr")
    }
  }

  def apply(find: V => Boolean): V = {
    list.find(find) match {
      case Some(value) => value
      case _ => throw new NoSuchElementException
    }
  }

}
