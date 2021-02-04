package scuff

import scala.collection.immutable.ArraySeq

package object json {
  @inline
  def unsafeWrap[T](arr: Array[T]): Seq[T] =
    ArraySeq unsafeWrapArray arr

  def fromMap(
      indexed: collection.MapView[Int, JsVal])(
      implicit
      config: JsVal.Config): JsArr =
    if (indexed.isEmpty) JsArr.Empty
    else {
      val size = indexed.keys.max + 1
      val arr = indexed.foldLeft(new Array[JsVal](size)) {
        case (arr, (offset, value)) =>
          arr(offset) = value
          arr
      }
      new JsArr(unsafeWrap(arr): _*)
    }


}
