package scuff

trait Subscription {
  def cancel()
}

trait Faucet {
  /** Filter type. */
  type F
  /** Listener type */
  type L
  /** Start subscription. */
  def subscribe(s: L, include: F ⇒ Boolean = _ ⇒ true): Subscription
}
