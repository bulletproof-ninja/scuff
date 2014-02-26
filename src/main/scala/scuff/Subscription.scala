package scuff

trait Subscription {
  def cancel()
}

trait Faucet {
  type F
  type L
  def subscribe(s: L, include: F ⇒ Boolean = _ ⇒ true): Subscription
}
