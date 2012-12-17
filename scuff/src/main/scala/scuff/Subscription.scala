package scuff

trait Topic {
  type T
  def subscribe(s: T ⇒ Unit): Subscription
  trait Subscription {
    def cancel()
  }
}