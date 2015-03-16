package scuff

package object reflect {
  private[scuff] val primitiveToWrapper: Map[Class[_], Class[_]] = Map(
    classOf[Boolean] -> classOf[java.lang.Boolean],
    classOf[Char] -> classOf[java.lang.Character],
    classOf[Short] -> classOf[java.lang.Short],
    classOf[Byte] -> classOf[java.lang.Byte],
    classOf[Int] -> classOf[java.lang.Integer],
    classOf[Long] -> classOf[java.lang.Long],
    classOf[Double] -> classOf[java.lang.Double],
    classOf[Float] -> classOf[java.lang.Float])
}
