package scuff

/** Basic monoid. */
trait Monoid[@specialized(Float, Double, Int, Long) T] extends ((T, T) => T) {
  def identity: T
  def op(a: T, b: T): T
  @inline final def apply(a: T, b: T): T = op(a, b)
}

final object Monoid {

  implicit final class MonoidExt[T](private val m: Monoid[T]) {
    def reduce(i: Iterable[T]): T = i.foldLeft(m.identity)(m.op)
  }

  def apply[T](ident: => T)(impl: (T, T) => T): Monoid[T] = new Monoid[T] {
    def identity = ident
    def op(a: T, b: T) = impl(a, b)
  }

  private[this] val list = new Monoid[List[Any]] {
    def identity = Nil
    def op(a: List[Any], b: List[Any]) = a ++ b
  }
  implicit def List[T] = list.asInstanceOf[Monoid[List[T]]]

  implicit val Unit: Monoid[Unit] = new Monoid[Unit] {
    def identity = ()
    def op(a: Unit, b: Unit) = ()
  }
  val Void: Monoid[Void] = new Monoid[Void] {
    def identity = null
    def op(a: Void, b: Void) = null
  }

  private[this] val hashSet = new Monoid[collection.immutable.HashSet[Any]] {
    def identity = collection.immutable.HashSet.empty[Any]
    def op(a: collection.immutable.HashSet[Any], b: collection.immutable.HashSet[Any]) = a ++ b
  }
  implicit def HashSet[T] = hashSet.asInstanceOf[Monoid[collection.immutable.HashSet[T]]]

  def IntMap[V](combine: (V, V) => V): Monoid[collection.immutable.IntMap[V]] = new Monoid[collection.immutable.IntMap[V]] {
    def identity = collection.immutable.IntMap.empty[V]
    def op(a: collection.immutable.IntMap[V], b: collection.immutable.IntMap[V]) = {
      a.unionWith[V](b, {
        case (_, va, vb) => combine(va, vb)
      })
    }
  }

  def LongMap[V](combine: (V, V) => V): Monoid[collection.immutable.LongMap[V]] = new Monoid[collection.immutable.LongMap[V]] {
    def identity = collection.immutable.LongMap.empty[V]
    def op(a: collection.immutable.LongMap[V], b: collection.immutable.LongMap[V]) = {
      a.unionWith[V](b, {
        case (_, va, vb) => combine(va, vb)
      })
    }
  }

  def HashMap[K, V](combine: (V, V) => V): Monoid[collection.immutable.HashMap[K, V]] = new Monoid[collection.immutable.HashMap[K, V]] {
    def identity = collection.immutable.HashMap.empty[K, V]
    def op(a: collection.immutable.HashMap[K, V], b: collection.immutable.HashMap[K, V]) = {
      a.merged[V](b) {
        case (ta, tb) => ta._1 -> combine(ta._2, tb._2)
      }
    }
  }

  def Sum[N](implicit n: Numeric[N]): Monoid[N] = new Monoid[N] {
    def identity = n.zero
    def op(a: N, b: N) = n.plus(a, b)
  }
  def Product[N](implicit n: Numeric[N]): Monoid[N] = new Monoid[N] {
    def identity = n.one
    def op(a: N, b: N) = n.times(a, b)
  }
}
