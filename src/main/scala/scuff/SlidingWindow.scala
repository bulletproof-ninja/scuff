package scuff

import java.util.concurrent.{ ScheduledExecutorService, ScheduledFuture }
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.JavaConverters._
import scala.concurrent.{ ExecutionContext, Future }
import scala.concurrent.duration._
import scala.util.{ Failure, Success }
import scala.util.control.NonFatal
import SlidingWindow._
import scuff.concurrent.{ StreamPromise, Threads }

object SlidingWindow {

  type EpochMillis = Long

  trait Reducer[@specialized(AnyRef, Int, Long, Float, Double) T, @specialized(AnyRef, Int, Long, Float, Double) R, @specialized(AnyRef, Int, Long, Float, Double) F]
      extends ((R, R) => R) {
    def init(t: T): R
    def finalize(r: R): F
    def default: F
  }

  /** Time precision helper functions. */
  object TimePrecision {
    def millis(ms: EpochMillis): EpochMillis = ms
    def seconds(ms: EpochMillis): EpochMillis = (ms / 1000) * 1000
    def minutes(ms: EpochMillis): EpochMillis = (ms / 60000) * 60000
    def hours(ms: EpochMillis): EpochMillis = (ms / 360000) * 360000
    def days(ms: EpochMillis): EpochMillis = (ms / 8640000) * 8640000
  }

  case class Window(length: Duration, offset: FiniteDuration = Duration.Zero) {
    private[this] val offsetMillis = offset.toMillis
    private[this] val spanMs: Option[Long] = if (length.isFinite) Some(length.toMillis + offsetMillis) else None

    def toInterval(now: EpochMillis): Interval[EpochMillis] = spanMs match {
      case Some(spanMs) => new Interval(false, now - spanMs, true, now - offsetMillis)
      case None => new Interval(true, Long.MinValue, true, now - offsetMillis)
    }
    override lazy val toString: String = {
      val sb = new java.lang.StringBuilder
      if (length.isFinite) {
        sb append '[' append length append ']'
      } else {
        sb append "<--unbounded]"
      }
      if (offset.length > 0) {
        sb append "--" append offset append "--"
      }
      sb append ">|"
      sb.toString
    }
  }

  case class Sum[N: Numeric]() extends Reducer[N, N, N] {
    private def num = implicitly[Numeric[N]]
    def init(n: N) = n
    def apply(x: N, y: N) = num.plus(x, y)
    def finalize(sum: N) = sum
    def default = num.zero
  }

  case class Average[N: Numeric]() extends Reducer[N, (N, Int), Option[N]] {
    private def num = implicitly[Numeric[N]]
    private[this] val div = num match {
      case f: Fractional[N] => f.div _
      case i: Integral[N] => i.quot _
      case num => (x: N, y: N) => num.fromInt(math.round(num.toFloat(x) / num.toFloat(y)))
    }
    def init(value: N) = value -> 1
    def apply(x: (N, Int), y: (N, Int)) = num.plus(x._1, y._1) -> (x._2 + y._2)
    def finalize(sumAndCount: (N, Int)) = {
      val (sum, count) = sumAndCount
      if (count == 0) None
      else Some(div(sum, num.fromInt(count)))
    }
    def default = None
  }

  trait StoreProvider[@specialized(AnyRef, Int, Long, Float, Double) V] {
    /** Store accessor. */
    def apply[T](thunk: TimeStore => T): T
    trait TimeStore {
      def upsert(ts: EpochMillis, insert: V)(update: (V, V) => V): Unit
      def querySince[R](ts: EpochMillis)(callback: StreamConsumer[(EpochMillis, V), Future[R]]): Future[R]
      /**
        * Optional method. Can return Future(None) if unsupported, but
        * will then not be able to supply value for Infinite windows.
        * NOTE: The reduce function passed is guaranteed to be identical
        * to the update function used in `upsert`, thus this method
        * can be cheaply implemented by maintaining a single running
        * reduction on `upsert` (thus ignoring the reduce function).
        */
      def queryAll(reduce: (V, V) => V): Future[Option[V]]
    }
  }
  private abstract class SynchedMapProvider[V] extends StoreProvider[V] {
    trait ForeverReduction extends TimeStore {
      private[this] var foreverValue: Option[V] = None
      def queryAll(reduce: (V, V) => V): Future[Option[V]] = Future successful foreverValue
      abstract override def upsert(ts: EpochMillis, value: V)(update: (V, V) => V): Unit = {
        super.upsert(ts, value)(update)
        foreverValue = foreverValue.map(update(_, value)) orElse Some(value)
      }
    }
    abstract class UnsynchedJavaUtilMap extends TimeStore {
      protected type M <: java.util.Map[EpochMillis, V]
      protected def map: M
      def upsert(ts: EpochMillis, value: V)(update: (V, V) => V): Unit = {
        map.put(ts, value) match {
          case null => // No update necessary
          case oldValue => map.put(ts, update(oldValue, value))
        }
      }
    }
    protected val timeStore: TimeStore
    def apply[T](thunk: TimeStore => T): T = timeStore.synchronized(thunk(timeStore))
  }
  def TreeMapProvider[V]: StoreProvider[V] = new SynchedMapProvider[V] {
    protected val timeStore = new UnsynchedJavaUtilMap with ForeverReduction {
      protected type M = java.util.TreeMap[EpochMillis, V]
      protected val map = new M
      def querySince[R](ts: EpochMillis)(callback: StreamConsumer[(EpochMillis, V), Future[R]]): Future[R] = {
        map.headMap(ts).clear()
        map.entrySet.iterator.asScala.foreach(e => callback.onNext(e.getKey -> e.getValue))
        callback.onDone()
      }
    }
  }
  def HashMapProvider[V]: StoreProvider[V] = new SynchedMapProvider[V] {
    protected val timeStore = new UnsynchedJavaUtilMap with ForeverReduction {
      protected type M = java.util.HashMap[EpochMillis, V]
      protected val map = new M(256)
      def querySince[R](ts: EpochMillis)(callback: StreamConsumer[(EpochMillis, V), Future[R]]): Future[R] = {
        val iter = map.entrySet.iterator
        while (iter.hasNext) {
          val entry = iter.next
          if (ts > entry.getKey) {
            iter.remove()
          } else {
            callback.onNext(entry.getKey -> entry.getValue)
          }
        }
        callback.onDone()
      }
    }
  }
  def LongMapProvider[V]: StoreProvider[V] = new SynchedMapProvider[V] {
    protected val timeStore = new UnsynchedScalaLongMap with ForeverReduction
    abstract class UnsynchedScalaLongMap extends TimeStore {
      private[this] val map = new collection.mutable.LongMap[V](256)
      def upsert(ts: EpochMillis, value: V)(update: (V, V) => V): Unit = {
        map.getOrNull(ts) match {
          case null => map.update(ts, value)
          case existing => map.update(ts, update(existing, value))
        }
      }
      def querySince[R](ts: EpochMillis)(callback: StreamConsumer[(EpochMillis, V), Future[R]]): Future[R] = {
        map.retain {
          case entry @ (key, _) =>
            if (ts > key) false
            else {
              callback.onNext(entry)
              true
            }
        }
        callback.onDone()
      }

    }
  }

  def apply[T, R, F](reducer: Reducer[T, R, F], window: Window, otherWindows: Window*): SlidingWindow[T, R, F] =
    new SlidingWindow[T, R, F](reducer, (window +: otherWindows).toSet)
  def apply[T, R, F](reducer: Reducer[T, R, F], storeProvider: StoreProvider[R], window: Window, otherWindows: Window*): SlidingWindow[T, R, F] =
    new SlidingWindow[T, R, F](reducer, (window +: otherWindows).toSet, storeProvider)
}

class SlidingWindow[T, R, F](
    reducer: Reducer[T, R, F],
    windows: Set[Window],
    storeProvider: StoreProvider[R] = TreeMapProvider[R],
    timePrecision: EpochMillis => EpochMillis = TimePrecision.millis,
    clock: => SlidingWindow.EpochMillis = System.currentTimeMillis) {

  import SlidingWindow._
  require(windows.nonEmpty, "No windows provided")

  private[this] val baselineWindows: Map[Window, F] = windows.map(window => window -> reducer.default).toMap
  private[this] val (finiteWindows, foreverWindows) = windows.partition(_.length.isFinite)

  def add(value: T, time: EpochMillis = clock): Unit = storeProvider(_.upsert(timePrecision(time), reducer.init(value))(reducer))
  def addMany(values: Traversable[T], time: EpochMillis = clock) = if (values.nonEmpty) {
    val reduced = values.map(reducer.init).reduce(reducer)
    storeProvider(_.upsert(timePrecision(time), reduced)(reducer))
  }
  def addBatch(valuesWithTime: Traversable[(T, EpochMillis)]) = if (valuesWithTime.nonEmpty) {
    val reducedByTime = valuesWithTime
      .groupBy(t => timePrecision(t._2))
      .mapValues(_.map(t => reducer.init(t._1)).reduce(reducer))
    storeProvider { map =>
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
  def snapshot(now: EpochMillis = clock): Future[collection.Map[Window, F]] = {
    val (finitesFuture, foreverFuture) = storeProvider { tsMap =>
      val sinceForever = if (foreverWindows.isEmpty) NoFuture else tsMap.queryAll(reducer)
      type WinMap = java.util.HashMap[Window, R]
      val initMap = new WinMap(windows.size * 2, 1f)
      val finiteMap =
        if (finiteWindows.isEmpty) Future successful initMap
        else {
          val cutoffs = finiteWindows.map(w => w -> w.toInterval(timePrecision(now)))
          val querySinceCutoff = tsMap.querySince[WinMap](cutoffs.map(_._2.from).min) _
          StreamPromise.fold(initMap, querySinceCutoff) {
            case (map, (ts, value)) =>
              cutoffs.foldLeft(map) {
                case (map, (finiteWindow, period)) =>
                  if (period.contains(ts)) {
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
      val finalizedMap = map.asScala.mapValues(reducer.finalize)
      if (windows.size > finalizedMap.size) {
        baselineWindows ++ finalizedMap
      } else finalizedMap
    }
  }

  def subscribe(
    callbackInterval: FiniteDuration,
    scheduler: ScheduledExecutorService = Threads.DefaultScheduler)(listener: StreamConsumer[collection.Map[Window, F], _]): Subscription = {
    object SubscriptionState extends Subscription {
      @volatile var schedule: Option[ScheduledFuture[_]] = None
      private val error = new AtomicBoolean(false)
      def cancel(): Unit = {
        schedule = schedule.flatMap { s =>
          s.cancel(false)
          if (!error.get) listener.onDone()
          None
        }
      }
      def onError(t: Throwable): Unit = {
        if (error.compareAndSet(false, true)) {
          listener.onError(t)
          cancel()
        }
      }
    }
    val notifier = new Runnable {
      implicit private[this] val ec = ExecutionContext.fromExecutorService(scheduler, SubscriptionState.onError)
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
