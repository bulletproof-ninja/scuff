package scuff

import concurrent.{ Threads, StreamCallback, StreamResult }
import java.util.concurrent.ScheduledExecutorService
import scala.concurrent.Future
import scala.concurrent.duration._
import SlidingWindow._
import scala.concurrent.ExecutionContext
import java.util.concurrent.atomic.AtomicReference
import scala.util.control.NonFatal
import scala.util._
import collection.JavaConverters._
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.ScheduledFuture

object SlidingWindow {

  type Timestamp = Long

  @specialized(Int, Long, Float, Double)
  trait Reducer[-T, R, F] extends ((R, R) => R) {
    def init(t: T): R
    def apply(x: R, y: R): R
    def finalize(r: R): Option[F]
    def default: Option[F] = None
  }

  case class Sum[N: Numeric]() extends Reducer[N, N, N] {
    private[this] val n = implicitly[Numeric[N]]
    def init(n: N) = n
    def apply(x: N, y: N) = n.plus(x, y)
    def finalize(sum: N) = Some(sum)
  }

  case class Average[N: Numeric]() extends Reducer[N, (N, Int), N] {
    private[this] val n = implicitly[Numeric[N]]
    private[this] val div = n match {
      case f: Fractional[N] => f.div _
      case i: Integral[N] => i.quot _
      case _ => (x: N, y: N) => n.fromInt(n.toInt(x) / n.toInt(y))
    }
    def init(value: N) = value -> 1
    def apply(x: (N, Int), y: (N, Int)) = n.plus(x._1, y._1) -> (x._2 + y._2)
    def finalize(sumAndCount: (N, Int)) = {
      val (sum, count) = sumAndCount
      if (count == 0) None
      else Some(div(sum, n.fromInt(count)))
    }
  }

  trait MapProvider[@specialized(Int, Long, Float, Double) V] {
    /** Map accessor. */
    def apply[T](thunk: TimeMap => T): T
    trait TimeMap {
      def upsert(ts: Timestamp, value: V)(update: (V, V) => V)
      def querySince(ts: Timestamp)(callback: StreamCallback[(Timestamp, V)])
      /**
        * Optional method. Can return Future(None) if unsupported, but
        * will not be able to supply value for Infinite windows.
        * NOTE: The reduce function passed is guaranteed to be identical
        * to the update function used in `upsert`, thus this method
        * can be cheaply implemented by maintaining a single running
        * reduction on `upsert`.
        */
      def queryAll(reduce: (V, V) => V): Future[Option[V]]
    }
  }
  private abstract class SynchedMapProvider[V] extends MapProvider[V] {
    abstract class UnsynchedTimeMap extends TimeMap {
      protected type M <: java.util.Map[Timestamp, V]
      protected def map: M
      private[this] var foreverValue = null.asInstanceOf[V]
      def queryAll(reduce: (V, V) => V): Future[Option[V]] = Future successful Option(foreverValue)
      def upsert(ts: Timestamp, value: V)(update: (V, V) => V) {
        map.put(ts, value) match {
          case null => // No update necessary
          case oldValue => map.put(ts, update(oldValue, value))
        }
        foreverValue = foreverValue match {
          case null => value
          case oldValue => update(oldValue, value)
        }
      }
    }
    protected val timeMap: UnsynchedTimeMap
    def apply[T](thunk: TimeMap => T): T = timeMap.synchronized(thunk(timeMap))
  }
  def TreeMapProvider[V]: MapProvider[V] = new TreeMapProvider
  private class TreeMapProvider[V] extends SynchedMapProvider[V] {
    protected val timeMap = new UnsynchedTimeMap {
      protected type M = java.util.TreeMap[Timestamp, V]
      protected val map = new M
      def querySince(ts: Timestamp)(callback: StreamCallback[(Timestamp, V)]) {
        import collection.JavaConversions._
        map.headMap(ts).clear()
        map.entrySet().foreach(e => callback.onNext(e.getKey -> e.getValue))
        callback.onCompleted()
      }
    }
  }
  def HashMapProvider[V]: MapProvider[V] = new HashMapProvider
  private class HashMapProvider[V] extends SynchedMapProvider[V] {
    protected val timeMap = new UnsynchedTimeMap {
      protected type M = java.util.HashMap[Timestamp, V]
      protected val map = new M(256)
      def querySince(ts: Timestamp)(callback: StreamCallback[(Timestamp, V)]) {
        val iter = map.entrySet.iterator
        while (iter.hasNext) {
          val entry = iter.next
          if (ts > entry.getKey) {
            iter.remove()
          } else {
            callback.onNext(entry.getKey, entry.getValue)
          }
        }
        callback.onCompleted()
      }
    }
  }

  def apply[T, R, F](reducer: Reducer[T, R, F], window: Duration, otherWindows: FiniteDuration*): SlidingWindow[T, R, F] =
    new SlidingWindow[T, R, F](reducer, (window +: otherWindows).toSet)
  def apply[T, R, F](reducer: Reducer[T, R, F], mapProvider: MapProvider[R], window: Duration, otherWindows: FiniteDuration*): SlidingWindow[T, R, F] =
    new SlidingWindow[T, R, F](reducer, (window +: otherWindows).toSet, mapProvider)
}

class SlidingWindow[-T, R, F](
    reducer: Reducer[T, R, F],
    windows: Set[Duration],
    mapProvider: MapProvider[R] = TreeMapProvider[R],
    clock: => SlidingWindow.Timestamp = System.currentTimeMillis) {

  import SlidingWindow._
  require(windows.nonEmpty, "No windows provided")

  private[this] val (finiteWindows, foreverWindows) = windows.partition(_.isFinite)

  def add(value: T, time: Timestamp = clock): Unit = mapProvider(_.upsert(time, reducer.init(value))(reducer))
  def addMany(values: Traversable[T], time: Timestamp = clock) = if (values.nonEmpty) {
    val reduced = values.map(reducer.init).reduce(reducer)
    mapProvider(_.upsert(time, reduced)(reducer))
  }
  def addBatch(valuesWithTime: Traversable[(T, Timestamp)]) = if (valuesWithTime.nonEmpty) {
    val reducedByTime = valuesWithTime.groupBy(_._2).mapValues(_.map(t => reducer.init(t._1)).reduce(reducer))
    mapProvider { map =>
      reducedByTime.foreach {
        case (time, value) => map.upsert(time, value)(reducer)
      }
    }
  }
  private[this] val NoFuture = Future successful None
  /**
    * Take snapshot of the provided window(s), using the `Reducer`,
    * and return result. If a window has no entries, or if `finalize`
    * returns `None`, it will not be present in the map, unless the
    * `Reducer` supplies a `default` value.
    * If a timestamp is provided, it is expected to always be
    * increasing (or equal to previous).
    */
  def snapshot(now: Timestamp = clock): Future[Map[Duration, F]] = {
    val (finitesFuture, foreverFuture) = mapProvider { tsMap =>
      val sinceForever = if (foreverWindows.isEmpty) NoFuture else tsMap.queryAll(reducer)
      val initMap = new java.util.HashMap[Duration, R](windows.size * 2, 1f)
      val finiteMap =
        if (finiteWindows.isEmpty) Future successful initMap
        else {
          val cutoffs = finiteWindows.map(w => w -> (now - w.toMillis))
          val cutoffQuery = tsMap.querySince(cutoffs.map(_._2).min) _
          StreamResult.fold(cutoffQuery)(initMap) {
            case (map, (ts, value)) =>
              cutoffs.foldLeft(map) {
                case (map, (finiteWindow, cutoff)) =>
                  if (ts >= cutoff) {
                    val newValue = map.get(finiteWindow) match {
                      case null => value
                      case old => reducer(old, value)
                    }
                    map.put(finiteWindow, newValue)
                  }
                  map
              }
          }
        }
      finiteMap -> sinceForever
    }
      implicit def ec = Threads.PiggyBack
    for {
      map <- finitesFuture
      foreverOpt <- foreverFuture
    } yield {
      for {
        foreverValue <- foreverOpt
        foreverKey <- foreverWindows
      } map.put(foreverKey, foreverValue)
      val finalizedMap = map.asScala.flatMap {
        case (w, r) => reducer.finalize(r).map(v => w -> v)
      }.toMap
      reducer.default match {
        case Some(default) if windows.size > finalizedMap.size =>
          windows.diff(finalizedMap.keySet).foldLeft(finalizedMap) {
            case (map, missingWindow) => map.updated(missingWindow, default)
          }
        case _ => finalizedMap
      }
    }
  }

  def subscribe(
    callbackInterval: FiniteDuration,
    scheduler: ScheduledExecutorService = Threads.DefaultScheduler)(listener: StreamCallback[Map[Duration, F]]): Subscription = {
    object SubscriptionState extends Subscription {
      @volatile var schedule: Option[ScheduledFuture[_]] = None
      private val error = new AtomicBoolean(false)
      def cancel() {
        schedule = schedule.flatMap { s =>
          s.cancel(false)
          if (!error.get) listener.onCompleted()
          None
        }
      }
      def onError(t: Throwable) {
        if (error.compareAndSet(false, true)) {
          listener.onError(t)
          cancel()
        }
      }
    }
    implicit val ec = ExecutionContext.fromExecutorService(scheduler, SubscriptionState.onError)
    val notifier = new Runnable {
      def run = try {
        snapshot().onComplete {
          case Success(result) => listener.onNext(result)
          case Failure(e) => SubscriptionState.onError(e)
        }
      } catch {
        case NonFatal(e) => SubscriptionState.onError(e)
      }
    }
    SubscriptionState.schedule = Some(scheduler.scheduleAtFixedRate(notifier, callbackInterval.toMillis, callbackInterval.toMillis, MILLISECONDS))
    SubscriptionState
  }
}
