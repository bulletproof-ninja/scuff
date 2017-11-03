package scuff.concurrent

import scala.concurrent._

trait StreamPromise[-V, +R] extends StreamConsumer[V, R] {

  protected def delegate: StreamConsumer[V, R] = null
  private[this] val promise = Promise[R]
  final def future = promise.future
  final def onError(th: Throwable): Unit = {
    if (delegate != null) delegate.onError(th)
    promise failure th
  }
  final def onDone() = {
    val result = if (delegate != null) delegate.onDone() else this.result
    promise completeWith result
    result
  }

  protected def result: Future[R] = sys.error("Must override either `result` or `delegate`")

}

object StreamPromise {

  def fold[V, R](init: R, subscribe: StreamConsumer[V, R] => _)(f: (R, V) => R): Future[R] = {
    val callback = new StreamPromise[V, R] {
      private[this] var acc = init
      def onNext(value: V): Unit = acc = f(acc, value)
      override def result = Future successful acc
    }
    subscribe(callback)
    callback.future
  }
  def foreach[V](subscribe: StreamConsumer[V, Unit] => Unit)(next: V => Unit): Future[Unit] = {
    val callback = StreamPromise(next)
    subscribe(callback)
    callback.future
  }

  def apply[V](next: V => Unit): StreamPromise[V, Unit] = apply(())(next)
  def apply[V, R](lazyResult: => R)(next: V => Unit) = new StreamPromise[V, R] {
    def onNext(value: V) = next(value)
    override def result = Future(lazyResult)(Threads.PiggyBack)
  }
  def apply[V, R](consumer: StreamConsumer[V, R]): StreamPromise[V, R] = consumer match {
    case promise: StreamPromise[V, R] => promise
    case _ => new StreamPromise[V, R] {
      override def delegate = consumer
      def onNext(v: V) = consumer.onNext(v)
    }
  }
}
