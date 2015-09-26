package scuff

trait Subscription {
  def cancel()
}

trait Faucet {
  /** Filter type. */
  type Filter
  /** Consumer type */
  type Consumer
  /** Start subscription. */
  def subscribe(s: Consumer, include: Filter => Boolean = _ => true): Subscription
}
