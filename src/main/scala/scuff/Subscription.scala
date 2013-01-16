package scuff

trait Channel {
  type T
  def subscribe(s: T ⇒ Unit): Subscription
  trait Subscription {
    def cancel()
  }
}