package scuff

trait Subscription {
  def cancel()
}

trait Channel {
  type F
  type L
  def subscribe(s: L, filter: F ⇒ Boolean = f ⇒ true): Subscription
}
