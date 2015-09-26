package scuff

import scala.annotation.StaticAnnotation
import scala.reflect.runtime.universe._
import scala.collection.concurrent.TrieMap
import scala.reflect._
import scala.reflect.api.TypeCreator
import scala.reflect.api.Universe
import scala.annotation.ClassfileAnnotation

/**
 * Trait to provide an upgrade path to versioned types.
 */
trait ReplacedBy[T] {
  def upgrade(): T
}

/**
 * Version annotation.
 */
//class Version(val value: Short) extends version

// TODO: Methods should probably be macros to get compile time checking for annotation
object serialVersionUID {
//  private[this] val versionTable = new TrieMap[TypeTag[_], Short]
  private[this] val serialUIDTable = new TrieMap[Class[_], Long]
  def apply[T <: AnyRef](t: T): Long = apply(t.getClass)
//  {
//    val m = runtimeMirror(t.getClass.getClassLoader)
//    val tc = new TypeCreator {
//      def apply[U <: Universe with Singleton](m: scala.reflect.api.Mirror[U]): U#Type = {
//        m.staticClass(t.getClass.getName).selfType
//      }
//    }
//    val tt = TypeTag(m, tc)
//    apply(tt)
//  }
  //  def apply[T: TypeTag]: Short = {
  //    versionTable.getOrElseUpdate(typeTag[T], {
  //      typeTag[T].tpe.typeSymbol.annotations.find(ann => ann.tree.tpe == typeOf[version]).map {
  //        _.tree.children.tail.head.asInstanceOf[Literal].value.value.asInstanceOf[Short]
  //      } getOrElse {
  //        scala.util.Try(apply).toOption.flatMap { ser =>
  //          if (ser <= Short.MaxValue && ser >= Short.MinValue) {
  //            Some(ser.toShort)
  //          } else None
  //        } getOrElse {
  //          sys.error(s"${typeTag[T].tpe} is not annotated with @${typeOf[version]}")
  //        }
  //      }
  //    })
  //  }

  def apply(cls: Class[_]): Long = {
    serialUIDTable.getOrElseUpdate(cls, {
      scala.util.Try(cls.getDeclaredField("serialVersionUID")).map { serialField =>
        serialField.setAccessible(true)
        serialField.get(cls).asInstanceOf[Long]
      } getOrElse {
        sys.error(s"No static `serialVersionUID` defined on $cls")
      }
    })
  }

  def apply[T <: AnyRef: ClassTag]: Long = apply(classTag[T].runtimeClass)

}
