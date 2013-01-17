package scuff

trait Subscription {
  def cancel()
}

trait Channel {
  type T
  def subscribe(s: T ⇒ Unit): Subscription
}