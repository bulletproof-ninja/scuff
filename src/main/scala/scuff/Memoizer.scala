package scuff

/**
  * Class that guarantees exactly one instance created per
  * argument (based on `equals` equality).
  * NOTICE: [[scala.collection.concurrent.Map#getOrElse]] and
  * [[scala.collection.concurrent.Map#putIfAbsent]] can provide
  * similar behavior, but cannot guarantee a single instance per
  * key, due to inherent race conditions.
  */
class Memoizer[A, R](impl: A => R) {

  @volatile private var map: Map[A, R] = Map.empty

  private def getOrNull(arg: A) = map.getOrElse(arg, null.asInstanceOf[R])

  def apply(arg: A): R = {
    getOrNull(arg) match {
      case null =>
        this.synchronized {
          getOrNull(arg) match {
            case null =>
              val res = impl(arg)
              map = map.updated(arg, res)
              res
            case res => res
          }
        }
      case res => res
    }
  }

}
